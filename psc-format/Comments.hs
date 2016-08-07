{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Comments where

import Prelude

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.Comments (Comment(..))

instance Pretty Comment where
    pretty = \case
        LineComment s -> text "--" <> pretty s
        BlockComment s -> text "{-" <> pretty s <> text "-}"

    prettyList comments
        | null comments = empty
        | otherwise = vsep (fmap pretty comments) <> hardline
