{-# LANGUAGE LambdaCase #-}

module Pretty where

import Prelude ((.), ($), fmap)
import Text.PrettyPrint.ANSI.Leijen

(<|>) :: Doc -> Doc -> Doc
a <|> b = group $ flatAlt b a

infixl 5 <|>

listify :: [Doc] -> Doc
listify = cat . punctuate (comma <> space)

prettyEncloseSep :: Doc -> Doc -> [Doc] -> Doc
prettyEncloseSep left right = \case
    [] -> left <> right
    docs ->
        prettySingleLineList left right docs
        <|> prettyLongList left right docs

prettyShortList :: Doc -> Doc -> [Doc] -> Doc
prettyShortList left right = \case
    [] -> left <> right
    x : xs -> hcat (left <> x : fmap (comma </>) xs) <> right

prettySingleLineList :: Doc -> Doc -> [Doc] -> Doc
prettySingleLineList left right = \case
    [] -> left <> right
    x : xs -> hcat (left <> x : fmap (comma <+>) xs) <> right

prettyLongList :: Doc -> Doc -> [Doc] -> Doc
prettyLongList left right = \case
    [] -> left <> right
    x : xs -> vcat (empty : left <+> x : fmap (comma <+>) xs) <$> right
