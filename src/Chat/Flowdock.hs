{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Chat.Flowdock where
import Control.Exception
import Control.Monad
import Control.Lens hiding ((.=))
import Control.Lens.TH
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromChunks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Chat.Flowdock.Internal
-- REST API
{-
listUserFlows
listAllFlows
getFlow
getFlowById
createFlow
updateFlow
sendMessage
sendComment
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

data JSONError = JSONError String
  deriving (Show, Typeable)

instance Exception JSONError

addPath :: ByteString -> Request -> Request
addPath p r = r { path = path r <> p }

flowdockApiBaseRequest = let (Just r) = parseUrl "https://api.flowdock.com/v1/" in r { requestHeaders = ("Content-Type", "application/json") : requestHeaders r }

-- Push API
newtype Push = Push { flowApiToken :: Text }

data FlowdockClient a = FlowdockClient
  { clientAuth    :: a
  , clientManager :: Manager
  }

withFlowdockClient :: auth -> (FlowdockClient auth -> IO a) -> IO a
withFlowdockClient a f = withManager tlsManagerSettings $ \m -> f (FlowdockClient a m)

data InboxPushFormat = Html
  deriving (Read, Show)

instance ToJSON InboxPushFormat where
  toJSON = const $ String "html"

data Tag = UserTag Text | HashTag Text
  deriving (Read, Show)

instance ToJSON Tag where
  toJSON (UserTag t) = String ("@" <> t)
  toJSON (HashTag t) = String ("#" <> t)

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

newInboxMessage = InboxPushMessage "" "" "" "" Nothing Nothing Nothing (Just Html) Nothing Nothing

makeFields ''InboxPushMessage
jsonizeToSnake ''InboxPushMessage

pushToInbox :: FlowdockClient Push -> InboxPushMessage -> IO ()
pushToInbox (FlowdockClient (Push token) man) msg = do
  -- post $ encode $ Authenticated t m
  let req = (addPath ("messages/team_inbox/" <> encodeUtf8 token) flowdockApiBaseRequest)
              { method = "POST"
              , requestBody = RequestBodyLBS $ encode msg
              }
  withResponse req man $ \_ -> return ()

data ChatPushMessage = ChatPushMessage
  { _cpContent          :: Text
  , _cpExternalUserName :: Text
  , _cpTags             :: Maybe [Tag]
  , _cpMessageId        :: Maybe Text
  } deriving (Read, Show)

makeFields ''ChatPushMessage
jsonizeToSnake ''ChatPushMessage

pushToChat :: FlowdockClient Push -> ChatPushMessage -> IO ()
pushToChat (FlowdockClient (Push token) man) msg = do
  -- post $ encode $ Authenticated t m
  let req = (addPath ("messages/chat/" <> encodeUtf8 token) flowdockApiBaseRequest)
              { method = "POST"
              , requestBody = RequestBodyLBS $ encode msg
              }
  withResponse req man $ \_ -> return ()

-- Streaming API
{-
streamFlows
streamFlow
-}
