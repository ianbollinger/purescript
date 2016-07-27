module Literals where

import Prelude ((++), ($), fmap)
import Text.PrettyPrint.ANSI.Leijen
import Language.PureScript.AST.Literals (Literal (..))
import Pretty

instance Pretty a => Pretty (Literal a) where
    pretty (NumericLiteral id) = text "integer or double"
    pretty (StringLiteral s) = text ("\"" ++ s ++ "\"")
    pretty (CharLiteral c) = text ['\'', c, '\'']
    pretty (BooleanLiteral b) = text $ if b then "true" else "false"
    pretty (ArrayLiteral vs) = list $ fmap pretty vs
    pretty (ObjectLiteral os) = text "{" <+> listify (fmap (\(key, val) -> text key <+> text ":" <+> pretty val) os) <+> text "}"
