{-# LANGUAGE RecordWildCards #-}

module Symbols where

import Text.PrettyPrint.ANSI.Leijen (Doc, char, text)

import Config (Config(..))

leftArrow :: Config -> Doc
leftArrow Config{..}
    | configUnicode = char '←'
    | otherwise = text "<-"

rightArrow :: Config -> Doc
rightArrow Config{..}
    | configUnicode = char '→'
    | otherwise = text "->"

leftFatArrow :: Config -> Doc
leftFatArrow Config{..}
    | configUnicode = char '⇐'
    | otherwise = text "<="

rightFatArrow :: Config -> Doc
rightFatArrow Config{..}
    | configUnicode = char '⇒'
    | otherwise = text "=>"

doubleColon :: Config -> Doc
doubleColon Config{..}
    | configUnicode = char '∷'
    | otherwise = text "::"

pipe :: Doc
pipe = char '|'

tick :: Doc
tick = char '`'

at :: Doc
at = char '@'

underscore :: Doc
underscore = char '_'

forall :: Config -> Doc
forall Config{..}
    | configUnicode = char '∀'
    | otherwise = text "forall"
