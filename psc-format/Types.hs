{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Types
    ( prettyType
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
    PrettyPrintObject row -> prettyRowWith config '{' '}' row
    TypeConstructor (Qualified moduleName properName) ->
        case moduleName of
            Nothing -> pretty properName
            Just moduleN ->
                pretty moduleN <> dot <> text (runProperName properName)
    TUnknown u -> underscore <> text (show u)
    Skolem name s _ _ -> text (name ++ show s ++ "skolem")
    REmpty -> parens empty
    TypeApp (TypeConstructor (Qualified _ (ProperName "Record"))) s ->
        prettyRowWith config '{' '}' s
    TypeApp (TypeConstructor (Qualified _ (ProperName "Function"))) s ->
        prettyType config s </> rightArrow config
    TypeApp t s -> prettyType config t <+> prettyType config s
    row@RCons{} -> prettyRowWith config '(' ')' row
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
            forall config <+> typeVars <> dot <+> group (prettyType config typ)
            where
                typeVars = text (unwords (typeVar : vars))

prettyTypeList :: Config -> [(String, Maybe Kind)] -> Doc
prettyTypeList config = sep . fmap go
    where
        go (s, kind) = case kind of
            Nothing -> text s
            Just kind' ->
                text s <+> doubleColon config <+> prettyKind config kind'

prettyRowWith :: Config -> Char -> Char -> Type -> Doc
prettyRowWith config open close = uncurry listToDoc . toList []
    where
        tailToPs :: Type -> Doc
        tailToPs REmpty = empty
        tailToPs other = pipe <+> prettyType config other

        nameAndTypeToPs :: Char -> String -> Type -> Doc
        nameAndTypeToPs start name ty =
            text (start : ' ' : name)
            <+> doubleColon config
            <+> prettyType config ty

        listToDoc :: [(String, Type)] -> Type -> Doc
        listToDoc [] REmpty = text [open, close]
        listToDoc [] rest = char open <+> tailToPs rest <+> char close
        listToDoc ts rest =
            hardline
            <> vcat (zipWith (\(nm, ty) i -> nameAndTypeToPs (if i == 0 then open else ',') nm ty) ts [0 :: Int ..] ++ tail' rest ++ [char close])
            where
                tail' REmpty = []
                tail' _ = [tailToPs rest]

        toList :: [(String, Type)] -> Type -> ([(String, Type)], Type)
        toList tys (RCons name ty row) = toList ((name, ty) : tys) row
        toList tys r = (reverse tys, r)
