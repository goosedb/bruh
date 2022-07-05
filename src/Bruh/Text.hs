module Bruh.Text 
  ( toString
  , fromString
  , module T 
  ) where

import Data.Text as T

toString :: T.Text -> String
toString = T.unpack

fromString :: String -> T.Text
fromString = T.pack

