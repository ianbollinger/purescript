{-# LANGUAGE LambdaCase #-}

module Kind
    ( prettyKind
    ) where

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.Kinds (Kind(..))

import Config (Config)
import Symbols (rightArrow)

prettyKind :: Config -> Kind -> Doc
prettyKind config = \case
    KUnknown a -> char 'u' <> pretty a
    Star -> char '*'
    Bang -> char '!'
    Row kind -> char '#' <+> prettyKind config kind
    FunKind kind1 kind2 ->
        prettyKind config kind1
        </> rightArrow config
        <+> prettyKind config kind2
    Symbol -> text "Symbol"
