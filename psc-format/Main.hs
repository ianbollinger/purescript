-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Main (main) where

import           Prelude                                 hiding (lex)

import qualified Options.Applicative                     as Opts
import           Text.PrettyPrint.ANSI.Leijen            as PP

import qualified Language.PureScript                     as P
import           Language.PureScript.AST.Declarations    (Module (Module))
import           Language.PureScript.Parser.Declarations

import           Config
import           Names                                   ()
import           Declarations                            (prettyDecl)
import           Comments                                ()

vSpace :: Doc
vSpace = hardline <> hardline

pprintModule :: Module -> Doc
pprintModule (Module _sourceSpan comments moduleName declarations exports) =
    comments'
    <> text "module"
    <+> pretty moduleName
    <> exports'
    <+> text "where"
    <> vSpace
    <> vsep (fmap (prettyDecl True) declarations)
    <> hardline
    where
        comments'
            | null comments = empty
            | otherwise = vsep (fmap pretty comments) <> hardline
        exports' = case exports of
            Nothing -> empty
            Just refs ->
                space
                PP.<$> PP.indent indentationLevel (makeList (fmap pretty refs))
        makeList docs = case docs of
            [] -> parens PP.line
            x : xs ->
                parens (space <> vcat (x : fmap ((comma <> space) <>) xs) <> PP.line)

data Config = Config
  { _input  :: String
  , _output :: String }

config :: Opts.Parser Config
config = Config
     Opts.<$> Opts.strOption
         ( Opts.long "input"
        Opts.<> Opts.metavar ""
        Opts.<> Opts.help "specify path to input file" )
     Opts.<*> Opts.strOption
         ( Opts.long "output"
        Opts.<> Opts.metavar ""
        Opts.<> Opts.help "specify path to output file" )

runFormatter :: Config -> IO ()
runFormatter (Config i o) = do
    inputFile <- readFile i
    case parseModulesFromFiles id [("Main", inputFile)] of
            Right v -> do
                writeFile o $ displayS (renderPretty 0.9 80 . vsep $ fmap (\(_, m) -> pprintModule m) v) ""
            Left e ->
                putStrLn $ P.prettyPrintMultipleErrors P.defaultPPEOptions e

main :: IO ()
main = Opts.execParser opts >>= runFormatter
  where
    opts = Opts.info (Opts.helper <*> config)
      ( Opts.fullDesc
     Opts.<> Opts.progDesc "run this program to format a purs file. "
     Opts.<> Opts.header "psc-format - format purescript files" )
