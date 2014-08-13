{-# LANGUAGE FlexibleContexts #-}
module Chat.Flowdock.Internal where
import Control.Lens
import Data.Aeson.TH
import Data.Char
import Data.Maybe
import Language.Haskell.TH.Syntax (Q, Name, Dec)

jsonize :: Name -> Q [Dec]
jsonize = deriveJSON (defaultOptions { fieldLabelModifier = \x -> let (FieldRules _ _ f _) = defaultFieldRules in fromMaybe x $ f x
                                     , omitNothingFields = True })

jsonizeAll :: (Traversable t) => MonadicFold Q (t Name) [Dec]
jsonizeAll = traverse . act jsonize

snakeCase (a:b:c) | isAlpha a, isUpper b = a : '_' : snakeCase (toLower b : c)
snakeCase (a:b) = a : snakeCase b
snakeCase x = x

jsonizeSnake = deriveJSON (defaultOptions { fieldLabelModifier = \x -> let (FieldRules _ _ f _) = defaultFieldRules in maybe (snakeCase x) snakeCase $ f x
                                          , omitNothingFields = True })

jsonizeToSnake = deriveToJSON (defaultOptions { fieldLabelModifier = \x -> let (FieldRules _ _ f _) = defaultFieldRules in maybe (snakeCase x) snakeCase $ f x
                                              , omitNothingFields = True })

