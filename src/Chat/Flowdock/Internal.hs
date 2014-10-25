{-# LANGUAGE FlexibleContexts #-}
module Chat.Flowdock.Internal where
import           Control.Lens
import           Data.Aeson.TH
import           Data.Char
import           Language.Haskell.TH.Syntax (Dec, Name, Q)

adjustNames :: String -> String
adjustNames s = case snakeCase $ dropWhile (not . isUpper) s of
  [] -> []
  (x:xs) -> toLower x : xs

jsonize :: Name -> Q [Dec]
jsonize = deriveJSON (defaultOptions { fieldLabelModifier = adjustNames
                                     , omitNothingFields = True })

jsonizeAll :: (Traversable t) => MonadicFold Q (t Name) [Dec]
jsonizeAll = traverse . act jsonize

snakeCase :: String -> String
snakeCase (a:b:c) | isAlpha a, isUpper b = a : '_' : snakeCase (toLower b : c)
snakeCase (a:b) = a : snakeCase b
snakeCase x = x

jsonizeSnake :: Name -> Q [Dec]
jsonizeSnake = deriveJSON (defaultOptions { fieldLabelModifier = adjustNames
                                          , omitNothingFields = True })

jsonizeToSnake :: Name -> Q [Dec]
jsonizeToSnake = deriveToJSON (defaultOptions { fieldLabelModifier = adjustNames
                                              , omitNothingFields = True })

