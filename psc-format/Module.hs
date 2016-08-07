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

import Config
import Names ()
import Declarations (prettyDeclaration, prettyDeclarations)
import Comments ()

vSpace :: Doc
vSpace = hardline <> hardline

prettyModule :: Config -> Module -> Doc
prettyModule config@Config{..} (Module _ comments moduleName decls exports) =
    prettyList comments
    <> text "module"
    <+> pretty moduleName
    <> exports'
    <+> text "where"
    <> vSpace
    <> prettyDeclarations config (sortBy sorter imports')
    <> vSpace
    <> prettyTopLevelDeclarations config decls'
    <> hardline
    where
        (imports', decls') = span isImportDecl decls
        sorter decl1 decl2 = case (decl1, decl2) of
            (PositionedDeclaration _ _ (ImportDeclaration moduleName1 _ _), PositionedDeclaration _ _ (ImportDeclaration moduleName2 _ _))
                | name1 == "Prelude" -> LT
                | otherwise -> name1 `compare` P.runModuleName moduleName2
                where
                    name1 = P.runModuleName moduleName1
            _ -> GT
        exports' = case exports of
            Nothing -> empty
            Just refs ->
                space
                <$> indent configIndent (makeList (fmap pretty refs))
        makeList docs = case docs of
            [] -> parens line
            x : xs ->
                parens (space <> vcat (x : fmap ((comma <> space) <>) xs) <> line)

prettyTopLevelDeclarations :: Config -> [Declaration] -> Doc
prettyTopLevelDeclarations config = \case
    [] -> empty
    [decl] -> prettyDeclaration config decl
    x@(PositionedDeclaration _ _ TypeDeclaration{}) : y@(PositionedDeclaration _ _ ValueDeclaration{}) : xs ->
        prettyDeclaration config x
        <$> prettyDeclaration config y
        <$> hardline
        <> prettyTopLevelDeclarations config xs
    x : xs ->
        prettyDeclaration config x
        <$> hardline
        <> prettyTopLevelDeclarations config xs
