{-# LANGUAGE RecordWildCards #-}

module Module
    ( prettyModule
    ) where

import Prelude hiding ((<$>))
import Data.List (sortBy)

import qualified Language.PureScript as P
import Language.PureScript.AST.Declarations
import Language.PureScript.Parser.Declarations

import Text.PrettyPrint.ANSI.Leijen

import Config
import Names ()
import Declarations (prettyDeclaration)
import Comments ()

vSpace :: Doc
vSpace = hardline <> hardline

prettyModule :: Config -> Module -> Doc
prettyModule config@Config{..} (Module _ comments moduleName decls exports) =
    comments'
    <> text "module"
    <+> pretty moduleName
    <> exports'
    <+> text "where"
    <> vSpace
    <> vcat (fmap (prettyDeclaration config) (sortBy sorter imports'))
    <> vSpace
    <> prettyDeclarations config decls'
    <> hardline
    where
        (imports', decls') = span isImport decls
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
                <$> indent configIndent (makeList (fmap pretty refs))
        makeList docs = case docs of
            [] -> parens line
            x : xs ->
                parens (space <> vcat (x : fmap ((comma <> space) <>) xs) <> line)

prettyDeclarations :: Config -> [Declaration] -> Doc
prettyDeclarations config decls = case decls of
    [] -> empty
    [decl] -> prettyDeclaration config decl
    x@(PositionedDeclaration _ _ TypeDeclaration{}) : y@(PositionedDeclaration _ _ ValueDeclaration{}) : xs ->
        prettyDeclaration config x
        <$> prettyDeclaration config y
        <$> hardline
        <> prettyDeclarations config xs
    x : xs ->
        prettyDeclaration config x <$> hardline <> prettyDeclarations config xs
