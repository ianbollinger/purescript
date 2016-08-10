{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Module
    ( prettyModule
    ) where

import Prelude hiding ((<$>))
import Data.List (sortBy)

import qualified Language.PureScript as P
import Language.PureScript.AST.Declarations (Declaration(..), Module(..),
                                             isImportDecl)

import Text.PrettyPrint.ANSI.Leijen

import Config (Config(..))
import Names ()
import Declarations (prettyDeclaration, prettyDeclarations)
import Comments ()
import Pretty (prettyLongList)

prettyModule :: Config -> Module -> Doc
prettyModule config@Config{..} (Module _ comments moduleName decls exports) =
    prettyList comments
    <> text "module"
    <+> pretty moduleName
    <> exports'
    <+> text "where"
    <> hardline
    <> imports''
    <> decls''
    where
        imports'' = case imports' of
            [] -> empty
            _ ->
                hardline <> prettyDeclarations config (sortBy sorter imports') <> hardline
        decls'' =  case decls' of
            [] -> empty
            _ -> hardline <> prettyTopLevelDeclarations config decls' <> hardline
        (imports', decls') = span isImportDecl decls
        sorter decl1 decl2 = case (decl1, decl2) of
            (PositionedDeclaration _ _ (ImportDeclaration moduleName1 _ _), PositionedDeclaration _ _ (ImportDeclaration moduleName2 _ _))
                | name1 == "Prelude" -> LT
                | name2 == "Prelude" -> GT
                | otherwise -> name1 `compare` name2
                where
                    name1 = P.runModuleName moduleName1
                    name2 = P.runModuleName moduleName2
            _ -> GT
        exports' = case exports of
            Nothing -> empty
            Just refs ->
                nest configIndent (prettyLongList lparen rparen (fmap pretty refs))

prettyTopLevelDeclarations :: Config -> [Declaration] -> Doc
prettyTopLevelDeclarations config = \case
    [] -> empty
    [decl] -> prettyDeclaration config decl
    [x@(PositionedDeclaration _ _ TypeDeclaration{}), y@(PositionedDeclaration _ _ ValueDeclaration{})] ->
        prettyDeclaration config x <$> prettyDeclaration config y
    x@(PositionedDeclaration _ _ TypeDeclaration{}) : y@(PositionedDeclaration _ _ ValueDeclaration{}) : xs ->
        prettyDeclaration config x
        <$> prettyDeclaration config y
        <$> hardline
        <> prettyTopLevelDeclarations config xs
    x : xs ->
        prettyDeclaration config x
        <$> hardline
        <> prettyTopLevelDeclarations config xs
