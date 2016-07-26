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
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

module Main where

import           Prelude                                 hiding (lex)

import           Control.Monad
import           Control.Monad.Error.Class               (MonadError (..))
--import Control.Monad.Writer.Strict

import           Data.List                               (intersperse)

--import Options.Applicative ((<>))

import           Text.PrettyPrint.ANSI.Leijen            as PP


import Names
import Declarations
--import qualified Language.PureScript as P
--import qualified Paths_purescript as Paths

-- https://github.com/purescript/purescript/blob/f6f4de900a5e1705a3356e60b2d8d3589eb7d68d/src/Language/PureScript/Errors.hs#L1209-L1317

--import Language.PureScript.Make
import           Data.Maybe
import qualified Language.PureScript                     as P
import           Language.PureScript.AST.Binders
import           Language.PureScript.AST.Declarations    (Declaration (..),
                                                          DeclarationRef (..), DoNotationElement (..),
                                                          Expr (..), ImportDeclarationType (..),
                                                          Module (Module), TypeInstanceBody (..))
import           Language.PureScript.AST.Literals        (Literal (..))
import           Language.PureScript.AST.SourcePos       (SourcePos, SourceSpan)
import           Language.PureScript.Errors              as P
import qualified Language.PureScript.Kinds               as KK
import           Language.PureScript.Names
import           Language.PureScript.Parser.Declarations
import           Language.PureScript.Pretty.Common       (prettyPrintObjectKey)
import           Language.PureScript.Pretty.Types        (prettyPrintRowWith)
import           Language.PureScript.Types               (Type (..))
import qualified Options.Applicative                     as Opts

vSpace :: Doc
vSpace = PP.line <> PP.line

pprintModule :: Module -> Doc
pprintModule (Module sourceSpan comments moduleName declarations _) =
    text "module" <+> pretty moduleName <+> text "where" <> vSpace <> vsep (fmap pretty declarations)

data Config = Config
  { input  :: String
  , output :: String }

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
                let [(_, Module _ _ _ declarations _)] = v
                writeFile o $ displayS (renderPretty 0.9 120 . vsep $ fmap (\(_, m) -> pprintModule m) v) ""
            Left e ->
                putStrLn $ P.prettyPrintMultipleErrors P.defaultPPEOptions e
runFormatter _ = return ()

main :: IO ()
main = Opts.execParser opts >>= runFormatter
  where
    opts = Opts.info (Opts.helper <*> config)
      ( Opts.fullDesc
     Opts.<> Opts.progDesc "run this program to format a purs file. "
     Opts.<> Opts.header "psc-format - format purescript files" )
