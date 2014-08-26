{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-| A client library for the Flowdock API. Currently only implements
    the push API.
 -}
module Chat.Flowdock (
  -- * Using the client
  -- $client
  FlowdockClient,
  withFlowdockClient,
  -- ** Authentication types
  -- $auth
  Push(..),
  User(..),
  -- ** General types
  Flow(..),
  flow,
  organization,
  -- ** REST API
  MessageType,
  Message,
  MessageId(..),
  message,
  comment,
  flow,
  event,
  tags,
  externalUserName,
  uuid,
  Chat,
  content,
  FileUpload,
  content,
  contentType,
  Status,
  content,
  fileName,
  sendMessage,
  Comment,
  content,
  messageId,
  sendComment,
  -- ** Pushing messages to the inbox
  pushToInbox,
  newInboxPushMessage,
  MessageResponse,
  messageId,
  sent,
  app,
  -- ** Pushing messages to the chatroom
  pushToChat,
  Content,
  ExternalUserName,
  newChatPushMessage,
  -- ** Streaming events
  FlowFilter,
  allFlows,
  justFlows,
  StreamQuery,
  user,
  Event,
  active,
  streamOptions,
  streamFlow,
  streamFlows,
  -- ** Constructing messages
  -- *** InboxPushMessage fields
  InboxPushMessage,
  source,
  fromAddress,
  subject,
  fromName,
  replyTo,
  project,
  InboxPushFormat(..),
  format,
  link,
  -- *** ChatPushMessage fields
  ChatPushMessage,
  externalUserName,
  messageId,
  -- *** Common fields
  content,
  Tag(..),
  tags,
  -- *** Exception types
  JSONError(..),
  -- *** Lens field classes
  HasActive,
  HasApp,
  HasContent,
  HasContentType,
  HasEvent,
  HasExternalUserName,
  HasFileName,
  HasFlow,
  HasFormat,
  HasFromAddress,
  HasFromName,
  HasLink,
  HasMessageId,
  HasOrganization,
  HasProject,
  HasReplyTo,
  HasSent,
  HasSource,
  HasSubject,
  HasTags,
  HasUser,
  HasUuid
) where
import           Control.Applicative
import           Control.Exception
import           Control.Lens           hiding ((.=))
import           Control.Lens.TH
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BC
import           Data.ByteString.Lazy   (fromChunks)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           Data.UUID
import           Pipes
import qualified Pipes.Aeson            as A
import           Pipes.HTTP
import           Pipes.Parse

import           Chat.Flowdock.Internal
-- REST API
{-
listUserFlows
listAllFlows
getFlow
getFlowById
createFlow
updateFlow
-}

-- wrapper to work around orphan instance for ToJSON
newtype WrapUUID = Wrap UUID

instance ToJSON WrapUUID where
  toJSON (Wrap u) = String $ decodeUtf8 $ toASCIIBytes u

data Flow
  = QualifiedFlow
    { _qfOrganization :: Text
    , _qfFlow         :: Text
    }
  | FlowId Text

data Tag = UserTag Text | HashTag Text
  deriving (Read, Show)

instance ToJSON Tag where
  toJSON (UserTag t) = String ("@" <> t)
  toJSON (HashTag t) = String ("#" <> t)

data Message a = Message
  { _mFlow             :: Flow
  , _mEvent            :: a
  , _mTags             :: [Tag]
  , _mExternalUserName :: Maybe Text
  , _mUuid             :: Maybe UUID
  }

data Chat = Chat
  { _chatContent :: Text
  }

data FileUpload = FileUpload
  { _fContent     :: ByteString
  , _fContentType :: Text
  , _fFileName    :: Text
  }

newtype MessageId = MessageId Int
  deriving (Show, ToJSON, FromJSON)

data Comment = Comment
  { _commentContent   :: Text
  , _commentMessageId :: MessageId
  }

message qf e = Message qf e [] Nothing Nothing

comment = Comment

data Status = Status
  { _statusContent :: Text
  }

type Event = Object

newtype FlowFilter = FlowFilter (Maybe [Flow])

data StreamQuery a = StreamQuery
  { _sqSource :: a
  , _sqUser   :: Maybe Bool
  , _sqActive :: Maybe Bool
  }

data MessageResponse c = MessageResponse
  { _mrMessageId :: MessageId
  , _mrSent      :: Int
  , _mrApp       :: Text
  -- , _mr
  } deriving (Show)

instance FromJSON (MessageResponse r) where
  parseJSON (Object o) = MessageResponse <$> (MessageId <$> o .: "id") <*> o .: "sent" <*> o .: "app"
  parseJSON _ = mzero

makeFields ''Flow
makeFields ''Message
makeFields ''Chat
makeFields ''FileUpload
makeFields ''Comment
makeFields ''Status
makeFields ''StreamQuery
makeFields ''MessageResponse

class StreamParams a where
  streamParams :: a -> [(ByteString, Maybe ByteString)]

instance StreamParams Flow where
  streamParams = const []

instance StreamParams FlowFilter where
  streamParams (FlowFilter s) = [("filter", Just $ encodeUtf8 $ T.intercalate "," $ s ^.. _Just . traverse . to (\f -> (f ^. organization) <> "/" <> (f ^. flow)))]


class MessageType a where
  messageJSON :: Message a -> Value

instance MessageType Chat where
  messageJSON m = Object . HM.insert "content" (m ^. event . content . to toJSON) $ baseMessage "message" m

instance ToJSON (Message Comment) where
  toJSON m = let e = m ^. event in Object
    . HM.insert "content" (e ^. content . to toJSON)
    . HM.insert "message" (e ^. messageId . to toJSON)
    $ baseMessage "comment" m

instance MessageType Status where
  messageJSON m = Object . HM.insert "content" (m ^. event . content . to toJSON) $ baseMessage "status" m

instance MessageType FileUpload where
  messageJSON m = Object . HM.insert "content" co $ baseMessage "file" m
    where
      e = m ^. event
      co = object [ "data" .= decodeUtf8 (B64.encode (e ^. content))
                  , "content_type" .= (e ^. contentType)
                  , "file_name" .= (e ^. fileName)
                  ]

data InboxPushFormat = Html
  deriving (Read, Show)

instance ToJSON InboxPushFormat where
  toJSON = const $ String "html"

data InboxPushMessage = InboxPushMessage
  { _ipSource      :: Text
  , _ipFromAddress :: Text
  , _ipSubject     :: Text
  , _ipContent     :: Text
  , _ipFromName    :: Maybe Text
  , _ipReplyTo     :: Maybe Text
  , _ipProject     :: Maybe Text
  , _ipFormat      :: Maybe InboxPushFormat
  , _ipTags        :: Maybe [Tag]
  , _ipLink        :: Maybe Text
  } deriving (Read, Show)

data ChatPushMessage = ChatPushMessage
  { _cpContent          :: Text
  , _cpExternalUserName :: Text
  , _cpTags             :: Maybe [Tag]
  , _cpMessageId        :: Maybe Text
  } deriving (Read, Show)

data JSONError = JSONError String
  deriving (Show, Typeable)

instance Exception JSONError

addPath :: ByteString -> Request -> Request
addPath p r = r { path = path r <> p }

flowdockRestBaseRequest = let (Just r) = parseUrl "https://api.flowdock.com/" in r { requestHeaders = ("Content-Type", "application/json") : requestHeaders r }
flowdockPushBaseRequest = let (Just r) = parseUrl "https://api.flowdock.com/v1/" in r { requestHeaders = ("Content-Type", "application/json") : requestHeaders r }

flowdockStreamBaseRequest = let (Just r) = parseUrl "https://stream.flowdock.com/" in r { requestHeaders = ("Content-Type", "application/json") : requestHeaders r }

-- Push API
newtype Push = Push { pushFlowApiToken :: Text }

newtype User = User { userAccessToken :: Text }

class ClientManagerSettings a where
  managerSettings :: a -> ManagerSettings

instance ClientManagerSettings Push where
  managerSettings = const tlsManagerSettings

instance ClientManagerSettings User where
  managerSettings = const (tlsManagerSettings { managerResponseTimeout = Nothing })

data FlowdockClient a = FlowdockClient
  { clientAuth    :: a
  , clientManager :: Manager
  }

type Content = Text
type ExternalUserName = Text

baseMessage :: Text -> Message a -> HashMap Text Value
baseMessage e m = HM.fromList (("event", String e) : ("tags", m ^. tags . to toJSON) : catMaybes [mk externalUserName "external_user_name", mk (uuid . _Just . to Wrap) "uuid"])
  where
    mk l k = preview (l . jk k) m
    jk k = to (\x -> (k, toJSON x))



{-
listMessages
showMessage
editMessage
deleteMessage
listPrivateConversations
getPrivateConversation
updatePrivateConversation
sendPrivateMessage
listPrivateMessage
showPrivateMessage
listUsers
listFlowUsers
getUser
updateUser
addUserToFlow
listOrganizations
getOrganization
getOrganizationById
updateOrganization
listSources
getSource
createSource
deleteSource
listInvitations
getInvitation
createInvitation
deleteInvitation
downloadFile
listFiles
uploadFile
-}

-- $client
--
-- The Flowdock API has different authentication mechanisms
-- for different parts of the API. Functions that depend on
-- specific authentication data are tagged with the authentication
-- type necessary to use them.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Chat.Flowdock
-- >
-- > -- The push API uses the 'Push' authentication type
-- > main = withFlowdockClient (Push "YOUR_FLOW_TOKEN_HERE") $ \client -> do
-- >   let msg = newChatPushMessage "Hello World Bot" "Hello, world!"
-- >   pushToChat client msg
-- >

decodeAesonStream br = do
  mr <- (eitherDecode . fromChunks) <$> brConsume br
  case mr of
    Left e -> throw $ JSONError e
    Right r -> return r

withFlowdockClient :: ClientManagerSettings auth => auth -> (FlowdockClient auth -> IO a) -> IO a
withFlowdockClient a f = withManager (managerSettings a) $ \m -> f (FlowdockClient a m)

sendMessage :: MessageType a => FlowdockClient User -> Message a -> IO (MessageResponse a)
sendMessage (FlowdockClient (User token) man) m = do
  let req = applyBasicAuth (encodeUtf8 token) "" $ (addPath ("flows/" <> (m ^. flow . organization . to encodeUtf8) <> "/" <> (m ^. flow . flow . to encodeUtf8) <> "/messages") flowdockRestBaseRequest)
              { method = "POST"
              , requestBody = RequestBodyLBS $ encode $ messageJSON m
              }
  withResponse req man (decodeAesonStream . responseBody)

sendComment :: FlowdockClient User -> Message Comment -> IO (MessageResponse Comment)
sendComment (FlowdockClient (User token) man) m = do
  let req = applyBasicAuth (encodeUtf8 token) "" $ (addPath ("flows/" <> (m ^. flow . organization . to encodeUtf8) <> "/" <> (m ^. flow . flow . to encodeUtf8) <> "/messages/" <> (m ^. event . messageId . to (\(MessageId i) -> BC.pack (show i))) <> "/comments") flowdockRestBaseRequest)
              { method = "POST"
              , requestBody = RequestBodyLBS $ encode m
              }
  withResponse req man (decodeAesonStream . responseBody)


newInboxPushMessage = InboxPushMessage "" "" "" "" Nothing Nothing Nothing (Just Html) Nothing Nothing

makeFields ''InboxPushMessage
jsonizeToSnake ''InboxPushMessage

pushToInbox :: FlowdockClient Push -> InboxPushMessage -> IO ()
pushToInbox (FlowdockClient (Push token) man) msg = do
  -- post $ encode $ Authenticated t m
  let req = (addPath ("messages/team_inbox/" <> encodeUtf8 token) flowdockPushBaseRequest)
              { method = "POST"
              , requestBody = RequestBodyLBS $ encode msg
              }
  withResponse req man $ \_ -> return ()

makeFields ''ChatPushMessage
jsonizeToSnake ''ChatPushMessage

newChatPushMessage :: ExternalUserName -> Content -> ChatPushMessage
newChatPushMessage eun c = ChatPushMessage c eun Nothing Nothing

pushToChat :: FlowdockClient Push -> ChatPushMessage -> IO ()
pushToChat (FlowdockClient (Push token) man) msg = do
  -- post $ encode $ Authenticated t m
  let req = (addPath ("messages/chat/" <> encodeUtf8 token) flowdockPushBaseRequest)
              { method = "POST"
              , requestBody = RequestBodyLBS $ encode msg
              }
  withResponse req man $ \_ -> return ()

-- Streaming API
streamQueryString :: StreamParams a => StreamQuery a -> [(ByteString, Maybe ByteString)]
streamQueryString q = streamParams (q ^. source) ++ catMaybes [aq, uq]
  where
    aq = do
      a <- q ^. active
      return $ if a
        then ("active", Just "true")
        else ("active", Just "idle")
    uq = case q ^. user of
      Nothing -> Nothing
      Just u -> if u then Just ("user", Just "1") else Nothing

allFlows :: FlowFilter
allFlows = FlowFilter Nothing

justFlows :: [Flow] -> FlowFilter
justFlows = FlowFilter . Just

streamOptions :: a -> StreamQuery a
streamOptions x = StreamQuery x Nothing Nothing

streamFlow :: FlowdockClient User -> StreamQuery Flow -> (Producer Event IO () -> IO a) -> IO a
streamFlow (FlowdockClient (User token) man) q cb = do
  let req = applyBasicAuth (encodeUtf8 token) "" $ addPath ("flows/" <> org <> "/" <> flow) flowdockStreamBaseRequest
  withHTTP req man $ \r -> do
    let responseStream = responseBody r
    cb $ streamJSON responseStream
  where
    org  = encodeUtf8 $ _qfOrganization $ _sqSource q
    flow = encodeUtf8 $ _qfFlow $ _sqSource q

streamFlows :: FlowdockClient User -> StreamQuery FlowFilter -> (Producer Event IO () -> IO a) -> IO a
streamFlows (FlowdockClient (User token) man) q cb = do
  let req = setQueryString (streamQueryString q) $ applyBasicAuth (encodeUtf8 token) "" $ addPath "flows" flowdockStreamBaseRequest
  withHTTP req man $ \r -> do
    let responseStream = responseBody r
    cb $ streamJSON responseStream

parseJSONStream :: Monad m => Parser ByteString m (Maybe (Either A.DecodingError Object))
parseJSONStream = do
  mStr <- draw
  case mStr of
    Nothing -> return ()
    Just str -> unDraw $ BC.dropWhile (== '\n') str
  A.decode

streamJSON :: MonadIO m => Producer ByteString m () -> Producer Object m ()
streamJSON = go
  where
    go p = do
      (r, p') <- lift $ runStateT parseJSONStream p
      case r of
        Nothing -> return ()
        Just v -> do
          case v of
            Left err -> liftIO $ putStrLn ("Decoding error: " ++ show err)
            Right ok -> yield ok
          go p'


getParentMessageId :: MessageResponse Comment -> MessageId
getParentMessageId = undefined
