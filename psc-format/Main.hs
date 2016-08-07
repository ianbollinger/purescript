{-# LANGUAGE RecordWildCards #-}

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

import Prelude
import System.Exit (exitFailure)
import System.IO (stderr, stdout, hPutStrLn, hSetEncoding, utf8)

import Text.PrettyPrint.ANSI.Leijen (displayS, renderPretty, vsep)
import Options.Applicative

import qualified Language.PureScript as P

import Config (Config(..))
import Module (prettyModule)

config :: Parser Config
config = Config
    <$> strArgument
        ( metavar "FILE"
        <> help "Path to input file"
        )
    <*> optional
        ( strOption
            ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Path to output file"
            )
        )
    <*> option auto
        ( long "indent"
        <> short 'i'
        <> metavar "WIDTH"
        <> value 2
        <> help "Indentation level (default: 2)"
        )
    <*> option auto
        ( long "width"
        <> short 'w'
        <> metavar "WIDTH"
        <> value 80
        <> help "Column at which to wrap long lines (default: 80)"
        )
    <*> switch
        ( long "unicode"
        <> short 'u'
        <> help "Use Unicode symbols where possible"
        )

runFormatter :: Config -> IO ()
runFormatter config'@Config{..} = do
    inputFile <- readFile configInput
    case P.parseModulesFromFiles id [("Main", inputFile)] of
        Right v ->
            case configOutput of
                Nothing -> putStr output
                Just o' -> writeFile o' output
            where
                output =
                    displayS (renderPretty 0.95 configWidth $ vsep $ fmap (\(_, m) -> prettyModule config' m) v) ""
        Left e -> do
            hPutStrLn stderr (P.prettyPrintMultipleErrors P.defaultPPEOptions e)
            exitFailure

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    execParser opts >>= runFormatter
    where
        opts = info (helper <*> config)
            ( fullDesc
            <> progDesc "Run this program to format a purs file."
            <> header "psc-format - Format PureScript files"
            )
