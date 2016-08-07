{-# LANGUAGE LambdaCase #-}

module Pretty where

import Prelude ((.), ($), fmap)
import Text.PrettyPrint.ANSI.Leijen

(<|>) :: Doc -> Doc -> Doc
a <|> b = group $ flatAlt b a

infixl 5 <|>

prettyTupled :: [Doc] -> Doc
prettyTupled docs = wide parens docs <|> skinny parens docs

wide :: (Doc -> Doc) -> [Doc] -> Doc
wide surround = surround . hcat . punctuate (text ", ")

skinny :: (Doc -> Doc) -> [Doc] -> Doc
skinny surround =
    surround . hcat . punctuate comma . fmap (\x -> space <> x <> linebreak)

listify :: [Doc] -> Doc
listify = cat . punctuate (comma <> space)

prettyEncloseSep :: Doc -> Doc -> [Doc] -> Doc
prettyEncloseSep left right docs = case docs of
    [] -> left <> right
    x : xs ->
        prettyShortList left right docs
        <|> prettyLongList left right docs

prettyShortList :: Doc -> Doc -> [Doc] -> Doc
prettyShortList left right = \case
    [] -> left <> right
    x : xs -> hcat (left <> x : fmap (comma </>) xs) <> right

prettyLongList :: Doc -> Doc -> [Doc] -> Doc
prettyLongList left right = \case
    [] -> left <> right
    x : xs -> vcat (empty : left <+> x : fmap (comma <+>) xs) <$> right
