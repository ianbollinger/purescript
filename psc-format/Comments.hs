{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Comments
    ( prettyComments
    ) where

import Prelude

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.Comments (Comment(..))

instance Pretty Comment where
    pretty = \case
        LineComment s -> text "--" <> pretty s
        BlockComment s -> text "{-" <> pretty s <> text "-}"

    prettyList = prettyComments empty

prettyComments :: Doc -> [Comment] -> Doc
prettyComments before = \case
    [] -> empty
    comments -> before <> vsep (fmap pretty comments) <> hardline
