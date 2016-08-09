{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Types
    ( prettyType
    , prettyShortType
    , prettyLongType
    , prettyLongTypes
    , prettyFunctionType
    , prettyTypes
    , prettyConstraint
    , prettyConstraints
    , prettyTypeList
    ) where

import Prelude hiding ((<$>))

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.Environment (tyFunction, tyRecord)
import Language.PureScript.Types (Constraint(..), Type(..), everywhereOnTypes,
                                  everywhereOnTypesTopDown)
import Language.PureScript.Names (Qualified(Qualified), runProperName)
import Language.PureScript.Kinds (Kind)
import Language.PureScript.Pretty.Common (prettyPrintObjectKey)

import Config (Config(..))
import Kind (prettyKind)
import Names ()
import Pretty (prettySingleLineList)
import Symbols (discretionarySpace, doubleColon, forall, pipe, rightArrow,
                rightFatArrow, underscore)

prettyType :: Config -> Type -> Doc
prettyType config@Config{..} = \case
    TypeWildcard _ -> underscore
    TypeVar var -> text var
    TypeLevelString s -> dquotes (text s)
    PrettyPrintObject row ->
        prettyRow config (lbrace <> space) (space <> rbrace) row
    TypeConstructor (Qualified moduleName properName) ->
        case moduleName of
            Nothing -> pretty properName
            Just moduleN ->
                pretty moduleN <> dot <> text (runProperName properName)
    TUnknown u -> underscore <> int u
    Skolem name s _ _ -> text (name ++ show s ++ "skolem")
    REmpty -> parens empty
    app@TypeApp{} ->
        nest (2 * configIndent) (group (go app))
        where
            go = \case
                TypeApp t s -> go t <$> prettyType config s
                t -> prettyType config t
    row@RCons{} -> prettyRow config lparen rparen row
    TypeOp op -> pretty op
    BinaryNoParensType op l r ->
        prettyType config l <+> prettyType config op <+> prettyType config r
    ParensInType typ ->
        nest configIndent
            (prettyFunctionType config (lparen <> flatAlt space empty) (linebreak <> discretionarySpace config <> rparen) typ)
    ForAll s t _ -> prettyForAll config s t []
    ConstrainedType constraints typ ->
        prettyConstraints config empty space rightFatArrow constraints
        <+> prettyType config typ
    KindedType typ kind ->
        prettyType config typ <+> doubleColon config <+> prettyKind config kind
    PrettyPrintFunction typ1 typ2 ->
        prettyType config typ1 <+> rightArrow config <+> prettyType config typ2
    PrettyPrintForAll xs typ ->
        forall config <+> typeVars <> dot <+> prettyType config typ
        where
            typeVars = hsep (fmap text xs)

-- Copied from Language.PureScript.Pretty.Types.
insertPlaceholders :: Type -> Type
insertPlaceholders = everywhereOnTypesTopDown convertForAlls . everywhereOnTypes convert
  where
  convert (TypeApp (TypeApp f arg) ret) | f == tyFunction = PrettyPrintFunction arg ret
  convert (TypeApp o r) | o == tyRecord = PrettyPrintObject r
  convert other = other
  convertForAlls (ForAll ident ty _) = go [ident] ty
    where
    go idents (ForAll ident' ty' _) = go (ident' : idents) ty'
    go idents other = PrettyPrintForAll idents other
  convertForAlls other = other

-- TODO: don't insertPlaceholders twice!
prettyFunctionType :: Config -> Doc -> Doc -> Type -> Doc
prettyFunctionType config before after typ' =
    group (before <> prettyPrint (insertPlaceholders typ') <> after)
    where
        prettyPrint = \case
            PrettyPrintForAll xs typ ->
                forall config
                <+> hsep (fmap text xs)
                <$$> discretionarySpace config
                <> dot
                <+> prettyPrint typ
            ConstrainedType constraints typ ->
                prettyConstraints config empty line rightFatArrow constraints
                <+> prettyPrint typ
            PrettyPrintFunction a b ->
                prettyType config a <$> rightArrow config <+> prettyPrint b
            BinaryNoParensType op l r ->
                prettyType config l
                <$> prettyType config op
                <+> prettyType config r
            t -> prettyType config t

prettyTypes :: Config -> [Type] -> Doc
prettyTypes config types
    | null types = empty
    | otherwise = space <> hsep (fmap (prettyType config) types)

prettyShortType :: Config -> Type -> Doc
prettyShortType config = prettyType config . insertPlaceholders

prettyLongType :: Config -> Type -> Doc
prettyLongType config typ = case insertPlaceholders typ of
    PrettyPrintObject row -> prettyLongRow config lbrace rbrace row
    row@RCons{} -> prettyLongRow config lparen rparen row
    x -> prettyType config x

prettyLongTypes :: Config -> [Type] -> Doc
prettyLongTypes config types
    | null types = empty
    | otherwise = space <> hsep (fmap (prettyLongType config) types)

prettyConstraint :: Config -> Constraint -> Doc
prettyConstraint config (Constraint class' args _) =
    pretty class' <> prettyTypes config args

prettyConstraints
    :: Config
    -> Doc
    -> Doc
    -> (Config -> Doc)
    -> [Constraint]
    -> Doc
prettyConstraints config begin preArrow arrow = \case
    [] -> empty
    [constraint] ->
        begin
        <> prettyConstraint config constraint
        <> preArrow
        <> arrow config
    constraints ->
        begin
        <> prettySingleLineList lparen rparen (fmap (prettyConstraint config) constraints)
        <> preArrow
        <> arrow config

prettyForAll :: Config -> String -> Type -> [String] -> Doc
prettyForAll config typeVar typ vars =
    case typ of
        ForAll s t _ ->
            prettyForAll config s t (typeVar : vars)
        _ ->
            forall config <+> typeVars <> dot <+> prettyType config typ
            where
                typeVars = text (unwords (typeVar : vars))

prettyTypeList :: Config -> [(String, Maybe Kind)] -> Doc
prettyTypeList config = sep . fmap go
    where
        go (s, kind) = case kind of
            Nothing -> text s
            Just kind' ->
                text s <+> doubleColon config <+> prettyKind config kind'

prettyLongRow :: Config -> Doc -> Doc -> Type -> Doc
prettyLongRow config open close = uncurry listToDoc . toList []
    where
        listToDoc :: [(String, Type)] -> Type -> Doc
        listToDoc [] REmpty = open <> close
        listToDoc [] rest = open <+> prettyTail config rest <+> close
        listToDoc ts rest =
            hardline
            <> vcat (zipWith go ts [0 :: Int ..])
            <> tail' rest
            <$> close
            where
                go (nm, ty) i =
                    (if i == 0 then open else comma)
                    <+> prettyNameAndType config nm ty
                tail' REmpty = empty
                tail' _ = line <> prettyTail config rest

prettyRow :: Config -> Doc -> Doc -> Type -> Doc
prettyRow config open close = uncurry listToDoc . toList []
    where
        listToDoc :: [(String, Type)] -> Type -> Doc
        listToDoc [] REmpty = open <> close
        listToDoc [] rest = open <+> prettyTail config rest <+> close
        listToDoc ts rest =
            hcat (zipWith go ts [0 :: Int ..]) <> tail' rest <> close
            where
                go (nm, ty) i =
                    (if i == 0 then open else comma <> space)
                    <> prettyNameAndType config nm ty
                tail' REmpty = empty
                tail' _ = space <> prettyTail config rest

prettyNameAndType :: Config -> String -> Type -> Doc
prettyNameAndType config name ty =
    text (prettyPrintObjectKey name)
    <+> doubleColon config
    <+> prettyType config ty

prettyTail :: Config -> Type -> Doc
prettyTail config = \case
    REmpty -> empty
    other -> pipe <+> prettyType config other

toList :: [(String, Type)] -> Type -> ([(String, Type)], Type)
toList tys = \case
    RCons name ty row -> toList ((name, ty) : tys) row
    r -> (reverse tys, r)
