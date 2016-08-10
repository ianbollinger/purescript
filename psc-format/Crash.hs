module Crash
  ( internalError
  ) where

import Prelude

-- |
-- Exit with an error message and a crash report link.
--
internalError :: String -> a
internalError =
  error
  . ("An internal error ocurred during formatting: " ++)
  . (++ "\nPlease report this at https://github.com/purescript/purescript/issues")
  . show
