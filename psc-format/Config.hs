module Config
    ( Config(..)
    ) where

import Prelude

data Config = Config
    { configInput :: String
    , configOutput :: Maybe String
    , configIndent :: Int
    , configWidth :: Int
    , configUnicode :: Bool
    }
