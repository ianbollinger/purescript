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
import Language.PureScript.AST.Literals (Literal (..))
import Language.PureScript.AST.SourcePos (SourcePos, SourceSpan)
import Language.PureScript.AST.Declarations (Module (Module), Declaration(..), Expr (..))

-- Language.PureScript.Names
instance Pretty (ProperName a) where
    pretty = text . runProperName

-- Language.PureScript.Names
instance Pretty ModuleName where
    pretty (ModuleName moduleName) =
        foldl (PP.<>) PP.empty
            $ intersperse moduleNameSeparator
            $ map pretty moduleName

moduleNameSeparator :: Doc
moduleNameSeparator = text "."

-- Language.PureScript.Names
instance Pretty Ident where
    pretty (Ident i) = text i
    pretty (Op o) = text o
    pretty (GenIdent mstring integer) = text "genIdent"

-- Language.PureScript.AST.Declarations
instance Pretty Declaration where
    pretty (DataDeclaration dataDeclType properName a b) = text "DataDeclaration"
    pretty (DataBindingGroupDeclaration declarations) = text "DataBindingGroupDeclaration"
    pretty (TypeSynonymDeclaration propertyName a typ) = text "TypeSynonymDeclaration"
    pretty (TypeDeclaration ident typ) = text "TypeDeclaration"
    pretty (ValueDeclaration ident nameKind binders expr) =
        let
            e =
                case expr of
                    Left guards -> text "ValueDeclaration - Guards"
                    Right expression -> pretty expression
        in
            pretty ident <+> text "=" <+> e
    pretty (BindingGroupDeclaration is) = text "BindingGroupDeclaration"
    pretty (ExternDeclaration tdent typ) = text "ExternDeclaration"
    pretty (ExternDataDeclaration properName kin) = text "ExternDataDeclaration"
    pretty (FixityDeclaration fixity string mqualified) = text "FixityDeclaration"
    pretty (ImportDeclaration moduleName importDeclarationType mmoduleName bool) = text "import" <+> pretty moduleName
    pretty (TypeClassDeclaration properName a constraints declarations) = text "TypeClassDeclaration"
    pretty (TypeInstanceDeclaration ident constraints qualified types typeInstanceBody) = text "TypeInstanceDeclaration"
    pretty (PositionedDeclaration sourceSpan comments declaration) = pretty declaration

pprintModule :: Module -> Doc
pprintModule (Module sourceSpan comments moduleName declarations _) =
    text "module" <+> pretty moduleName <+> text "where" <> PP.line <> vsep (fmap pretty declarations)

-- Language.PureScript.AST.Declarations
instance Pretty Expr where
    pretty (Literal literal) = pretty literal
    pretty (UnaryMinus expr) = text "-" <> pretty expr
    pretty (BinaryNoParens expr1 expr2 expr3) = pretty "BinaryNoParens"
    pretty (Parens expr) = text "(" <> pretty expr <> text ")"
    pretty (OperatorSection expr lr) = pretty "OperatorSection"
    pretty (ObjectGetter s) = text "_." <> text s
    pretty (Accessor s expr) = text "Accessor"
    pretty (ObjectUpdate expr ss) = text "ObjectUpdate"
    pretty (Abs l expr) = pretty "Abs"
    pretty (App expr1 expr2) = pretty expr1 <+> pretty expr2
    pretty (Var qualified) = pretty qualified
    pretty (IfThenElse expr1 expr2 expr3) = text "if" <+> pretty expr1 PP.<$> text "then" <+> pretty expr2 PP.<$> text "else" <+> pretty expr3
    pretty (Constructor qualified) = text "Constructor"
    pretty (Case exprs caseAlternatives) = text "Case"
    pretty (TypedValue bool expr typ) = text "TypedValue"
    pretty (Let declarations expr) = text "Let"
    pretty (Do doNotationElements) = text "Do"
    pretty (TypeClassDictionaryConstructorApp qualified expr) = text "TypeClassDictionaryConstructorApp"
    pretty (TypeClassDictionary constraint a) = text "TypeClassDictionary"
    pretty (TypeClassDictionaryAccessor qualified ident) = text "TypeClassDictionaryAccessor"
    pretty (SuperClassDictionary qualified types) = text "SuperClassDictionary"
    pretty AnonymousArgument = text "AnonymousArgument"
    pretty (PositionedValue sourceSpan comments expr) = pretty expr

-- Language.PureScript.Names
instance Pretty a => Pretty (Qualified a) where
    pretty (Qualified mN n) =
        pretty moduleName <> pretty n
        where
            moduleName =
                case mN of
                    Just name -> pretty name
                    Nothing -> text ""

-- Language.PureScript.AST.Literals
instance Pretty a => Pretty (Literal a) where
    pretty (NumericLiteral id) = text "integer or double"
    pretty (StringLiteral s) = text ("\"" ++ s ++ "\"")
    pretty (CharLiteral c) = text ['\'', c, '\'']
    pretty (BooleanLiteral b) = text $ if b then "true" else "false"
    pretty (ArrayLiteral vs) = text "[" <> text "]"
    pretty (ObjectLiteral os) = text "Object {}"

main :: IO ()
main = do
    let file = "module Main where\nimport Control.Monad.Eff.Console\nmain = log \"hello\""
    putStrLn file
    putStrLn "-----------"
    case parseModulesFromFiles id [("Main", file)] of
        Right v -> do
            let [(_, Module _ _ _ declarations _)] = v
            print declarations
            putStrLn "------------"
            putStrLn $ displayS (renderCompact $ vsep $ fmap (\(_, m) -> pprintModule m) v) ""
        Left e ->
            putStrLn $ P.prettyPrintMultipleErrors False e
