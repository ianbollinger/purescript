{-# LANGUAGE LambdaCase #-}

module Pretty where

import Prelude hiding ((<$>))

import Text.PrettyPrint.ANSI.Leijen

listify :: [Doc] -> Doc
listify = hcat . punctuate comma

prettyEncloseSep :: Doc -> Doc -> [Doc] -> Doc
prettyEncloseSep left right = \case
    -- TODO: an extra space can be inserted here.
    [] -> space <> group (left <> right)
    x : xs ->
        group (vcat (empty : left <> flatAlt space empty <> x : fmap (comma <+>) xs) <$$> right)

prettySingleLineList :: Doc -> Doc -> [Doc] -> Doc
prettySingleLineList left right = \case
    [] -> left <> right
    x : xs -> hcat (left <> x : fmap (comma <+>) xs) <> right

prettyLongList :: Doc -> Doc -> [Doc] -> Doc
prettyLongList left right = \case
    [] -> left <> right
    x : xs -> vcat (empty : left <+> x : fmap (comma <+>) xs) <$> right

spaceSeparatedList :: [Doc] -> Doc
spaceSeparatedList = \case
    [] -> empty
    xs -> space <> hsep xs
