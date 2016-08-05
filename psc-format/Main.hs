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

module Main
    ( main
    ) where

import           Prelude                                 hiding (lex, (<$>))
import           Data.List                               (sortBy)

import qualified Options.Applicative                     as Opts
import           Text.PrettyPrint.ANSI.Leijen            as PP

import qualified Language.PureScript                     as P
import           Language.PureScript.AST.Declarations
import           Language.PureScript.Parser.Declarations

import           Config
import           Names                                   ()
import           Declarations                            ()
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
    <> vcat (fmap pretty (sortBy sorter imports'))
    <> vSpace
    <> pprintDecls decls'
    <> hardline
    where
        (imports', decls') = span isImport declarations
        isImport decl = case decl of
            PositionedDeclaration _ _ ImportDeclaration{} -> True
            _ -> False
        sorter decl1 decl2 = case (decl1, decl2) of
            (PositionedDeclaration _ _ (ImportDeclaration moduleName1 _ _), PositionedDeclaration _ _ (ImportDeclaration moduleName2 _ _))
                | name1 == "Prelude" -> LT
                | otherwise -> name1 `compare` P.runModuleName moduleName2
                where
                    name1 = P.runModuleName moduleName1
            _ -> GT
        comments'
            | null comments = empty
            | otherwise = vsep (fmap pretty comments) <> hardline
        exports' = case exports of
            Nothing -> empty
            Just refs ->
                space
                <$> PP.indent indentationLevel (makeList (fmap pretty refs))
        makeList docs = case docs of
            [] -> parens PP.line
            x : xs ->
                parens (space <> vcat (x : fmap ((comma <> space) <>) xs) <> PP.line)

pprintDecls :: [Declaration] -> Doc
pprintDecls decls = case decls of
    [] -> empty
    [decl] -> pretty decl
    x@(PositionedDeclaration _ _ TypeDeclaration{}) : y@(PositionedDeclaration _ _ ValueDeclaration{}) : xs ->
        pretty x <$> pretty y <$> hardline <> pprintDecls xs
    x : xs ->
        pretty x <$> hardline <> pprintDecls xs

data Config = Config
  { _input  :: String
  , _output :: String
  }

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
