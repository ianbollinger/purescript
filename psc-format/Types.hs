{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Types
    ( prettyType
    , prettyLongType
    , prettyLongTypes
    , prettyTypes
    , prettyConstraint
    , prettyConstraints
    , prettyTypeList
    ) where

import Prelude hiding ((<$>))

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.Types (Constraint(..), Type(..))
import Language.PureScript.Names (ProperName(ProperName), Qualified(Qualified),
                                  runOpName, runProperName, showQualified)
import Language.PureScript.Kinds (Kind)
import Language.PureScript.Pretty.Common (prettyPrintObjectKey)

import Config (Config)
import Kind (prettyKind)
import Names ()
import Pretty (listify)
import Symbols (doubleColon, forall, pipe, rightArrow, rightFatArrow,
                underscore)

prettyType :: Config -> Type -> Doc
prettyType config = \case
    TypeWildcard _ -> underscore
    TypeVar var -> text var
    TypeLevelString s -> text (show s ++ "TypeLevelString")
    PrettyPrintObject row ->
        prettyRow config (lbrace <> space) (space <> rbrace) row
    TypeConstructor (Qualified moduleName properName) ->
        case moduleName of
            Nothing -> pretty properName
            Just moduleN ->
                pretty moduleN <> dot <> text (runProperName properName)
    TUnknown u -> underscore <> text (show u)
    Skolem name s _ _ -> text (name ++ show s ++ "skolem")
    REmpty -> parens empty
    TypeApp (TypeConstructor (Qualified _ (ProperName "Record"))) s ->
        prettyRow config (lbrace <> space) (space <> rbrace) s
    TypeApp (TypeConstructor (Qualified _ (ProperName "Function"))) s ->
        prettyType config s </> rightArrow config
    TypeApp t s -> prettyType config t <+> prettyType config s
    row@RCons{} -> prettyRow config lparen rparen row
    TypeOp op -> text (showQualified runOpName op)
    BinaryNoParensType op l r ->
        prettyType config l <+> prettyType config op <+> prettyType config r
    ParensInType typ -> parens (prettyType config typ)
    ForAll s t _ -> prettyForAll config s t []
    ConstrainedType constraints typ ->
        prettyConstraints config empty rightFatArrow constraints
        <+> prettyType config typ
    KindedType typ kind ->
        prettyType config typ <+> doubleColon config <+> prettyKind config kind
    PrettyPrintFunction _typ1 _typ2 -> text "PrettyPrintFunction"
    PrettyPrintForAll _xs _typ -> text "PrettyPrintForall"

prettyTypes :: Config -> [Type] -> Doc
prettyTypes config types
    | null types = empty
    | otherwise = space <> hsep (fmap (prettyType config) types)

prettyLongType :: Config -> Type -> Doc
prettyLongType config typ = case typ of
    PrettyPrintObject row -> prettyLongRow config lbrace rbrace row
    TypeApp (TypeConstructor (Qualified _ (ProperName "Record"))) s ->
        prettyLongRow config lbrace rbrace s
    row@RCons{} -> prettyLongRow config lparen rparen row
    _ -> prettyType config typ

prettyLongTypes :: Config -> [Type] -> Doc
prettyLongTypes config types
    | null types = empty
    | otherwise = space <> hsep (fmap (prettyLongType config) types)

prettyConstraint :: Config -> Constraint -> Doc
prettyConstraint config (Constraint class' args _) =
    pretty class' <> prettyTypes config args

prettyConstraints :: Config -> Doc -> (Config -> Doc) -> [Constraint] -> Doc
prettyConstraints config begin arrow constraints
    | null constraints = empty
    | length constraints == 1 =
        begin
        <> prettyConstraint config (head constraints)
        </> arrow config
    | otherwise =
        begin
        <> parens (listify (fmap (prettyConstraint config) constraints))
        </> arrow config

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
