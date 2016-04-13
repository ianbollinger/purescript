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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Prelude hiding (lex)

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
--import Control.Monad.Writer.Strict

import Data.List (isSuffixOf, partition, intersperse)
import Data.Version (showVersion)
import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as BU8

--import Options.Applicative ((<>))

import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.UTF8
import System.FilePath.Glob (glob)

import Text.PrettyPrint.Leijen as PP

--import qualified Language.PureScript as P
--import qualified Paths_purescript as Paths

--import Language.PureScript.Make
import Language.PureScript.Names
import Language.PureScript.Parser.Lexer
import Language.PureScript.Parser.Declarations
import Language.PureScript.Errors as P hiding ((<>))
import Language.PureScript.AST.SourcePos (SourcePos, SourceSpan)
import Language.PureScript.AST.Declarations (Module (Module))

instance Pretty (ProperName a) where
    pretty = text . runProperName

instance Pretty ModuleName where
    pretty (ModuleName moduleName) =
        foldl (PP.<>) PP.empty
            $ intersperse moduleNameSeparator
            $ map pretty moduleName

moduleNameSeparator :: Doc
moduleNameSeparator = text "."

-- instance Pretty Declaration where
--     definitions
--     DataDeclaration DataDeclType (ProperName 'TypeName) [(String, Maybe Kind)] [(ProperName 'ConstructorName, [Type])]
--  -- |
--  -- A minimal mutually recursive set of data type declarations
--  --
--  | DataBindingGroupDeclaration [Declaration]
--  -- |
--  -- A type synonym declaration (name, arguments, type)
--  --
--  | TypeSynonymDeclaration (ProperName 'TypeName) [(String, Maybe Kind)] Type
--  -- |
--  -- A type declaration for a value (name, ty)
--  --
--  | TypeDeclaration Ident Type
--  -- |
--  -- A value declaration (name, top-level binders, optional guard, value)
--  --
--  | ValueDeclaration Ident NameKind [Binder] (Either [(Guard, Expr)] Expr)
--  -- |
--  -- A minimal mutually recursive set of value declarations
--  --
--  | BindingGroupDeclaration [(Ident, NameKind, Expr)]
--  -- |
--  -- A foreign import declaration (name, type)
--  --
--  | ExternDeclaration Ident Type
--  -- |
--  -- A data type foreign import (name, kind)
--  --
--  | ExternDataDeclaration (ProperName 'TypeName) Kind
--  -- |
--  -- A fixity declaration (fixity data, operator name, value the operator is an alias for)
--  --
--  | FixityDeclaration Fixity String (Maybe (Either (Qualified Ident) (Qualified (ProperName 'ConstructorName))))
--  -- |
--  -- A module import (module name, qualified/unqualified/hiding, optional "qualified as" name)
--  -- TODO: also a boolean specifying whether the old `qualified` syntax was used, so a warning can be raised in desugaring (remove for 0.9)
--  --
--  | ImportDeclaration ModuleName ImportDeclarationType (Maybe ModuleName) Bool
--  -- |
--  -- A type class declaration (name, argument, implies, member declarations)
--  --
--  | TypeClassDeclaration (ProperName 'ClassName) [(String, Maybe Kind)] [Constraint] [Declaration]
--  -- |
--  -- A type instance declaration (name, dependencies, class name, instance types, member
--  -- declarations)
--  --
--  | TypeInstanceDeclaration Ident [Constraint] (Qualified (ProperName 'ClassName)) [Type] TypeInstanceBody
--  -- |
--  -- A declaration with source position information

pprintModule :: Module -> Doc
pprintModule (Module sourceSpan comments moduleName declarations _) =
    text "module" <+> pretty moduleName <> PP.line
--pprint ::

main :: IO ()
main = do
    let filename = "Main.hs"
    let content = "module Main where"
    let ts = lex filename content
    case parseModulesFromFiles id [("Main", "module Main where\nimport Control.Monad.Eff.Console\nmain = log \"hell\"")] of
        Right v -> do
            let [(_, (Module _ _ _ declarations b))] = v
            print declarations
            print b
            putStrLn "------------"
            print $ fmap (\(_, m) -> (displayS $ renderCompact $ pprintModule m) "") v
        Left e ->
            putStrLn $ P.prettyPrintMultipleErrors False e
    putStrLn "Hello"
    print ts
