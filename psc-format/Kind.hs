{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kind where

import Prelude
import Text.PrettyPrint.ANSI.Leijen as PP
import Language.PureScript.Kinds

instance Pretty Kind where
    pretty k =
        text ":" <+> text sign
        where
            sign =
                case k of
                    KUnknown a ->
                        show a
                    Star -> "*"
                    Bang -> "!"
                    Row kind -> "ROW KIND"
                    FunKind kind1 kind2 ->  "FUNKIND KIND KIND"
                    Symbol -> "SYMBOLL"