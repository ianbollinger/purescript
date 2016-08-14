{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Types
    ( prettyType
    , prettyShortType
    , prettyLongType
    , prettyTypes
    , prettyConstraints
    , prettyTypeList
    ) where

import Prelude hiding ((<$>))

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.Environment (tyFunction, tyRecord)
import Language.PureScript.Types (Constraint(..), Type(..), everywhereOnTypes,
                                  everywhereOnTypesTopDown)
import Language.PureScript.Kinds (Kind)
import Language.PureScript.Pretty.Common (prettyPrintObjectKey)

import Config (Config(..))
import Crash (internalError)
import Kind (prettyKind)
import Names ()
import Pretty (prettyEncloseSep, spaceSeparatedList)
import Symbols (discretionarySpace, doubleColon, forall, pipe, rightArrow,
                rightFatArrow, underscore)

prettyTypeAtom :: Config -> Doc -> Type -> Doc
prettyTypeAtom config@Config{..} before = \case
    TypeWildcard _ -> before <> underscore
    TypeVar var -> before <> text var
    TypeLevelString s -> before <> dquotes (text s)
    PrettyPrintObject row ->
        nest configIndent (prettyObjectType config (flatAlt linebreak before) row)
    row@RCons{} ->
        nest configIndent (prettyRowType config (flatAlt linebreak before) row)
    t@(TypeConstructor name)
        | t == tyFunction -> before <> text "(->)"
        | otherwise -> before <> pretty name
    REmpty -> before <> parens empty
    app@TypeApp{} -> before <> prettyTypeApp config 2 empty app
    TypeOp op -> before <> pretty op
    ParensInType app@TypeApp{} -> before <> parens (prettyTypeApp config 1 empty app)
    ParensInType typ ->
        before
        <> nest configIndent
            (prettyTypeComplex config lparen (flatAlt space empty) (linebreak <> discretionarySpace config <> rparen) typ)
    KindedType typ kind ->
        prettyTypeAtom config before typ
        <+> doubleColon config
        <+> prettyKind config kind
    ForAll{} -> internalError "ForAll type encountered."
    TUnknown _ -> internalError "TUnknown type encountered."
    Skolem{} -> internalError "Skolem type encountered."
    _ -> internalError "Non-atom type encountered."

-- |
-- `prettyPrintApp config indentationLevel typ` pretty-prints a type application
-- where `indentationLevel` is number of times the type arguments should be
-- indented if split over multiple lines.
--
prettyTypeApp :: Config -> Int -> Doc -> Type -> Doc
prettyTypeApp config@Config{..} indentationLevel before =
    group . (before <>) . nest (indentationLevel * configIndent) . go
    where
        go = \case
            TypeApp t s -> go t <$> prettyTypeNoIndent s
            t -> prettyTypeNoIndent t
        prettyTypeNoIndent = \case
            PrettyPrintObject row -> prettyObjectType config empty row
            row@RCons{} -> prettyRowType config empty row
            typ -> prettyTypeAtom config empty typ

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

isTypeAtom :: Type -> Bool
isTypeAtom = \case
    PrettyPrintForAll _ _ -> False
    ConstrainedType _ _ -> False
    PrettyPrintFunction _ _ -> False
    BinaryNoParensType{} -> False
    ParensInType _ -> False
    _ -> True

prettyTypeComplex :: Config -> Doc -> Doc -> Doc -> Type -> Doc
prettyTypeComplex config before space'' after typ' =
    group (before <> prettyPrint space'' typ' <> after)
    where
        prettyPrint space' = \case
            PrettyPrintForAll xs typ ->
                space'
                <> forall config
                <+> hsep (fmap text xs)
                <$$> discretionarySpace config
                <> dot
                <> prettyPrint space typ
            ConstrainedType constraints typ ->
                prettyConstraints config space' line rightFatArrow constraints
                <> prettyPrint space typ
            PrettyPrintFunction a b ->
                prettyTypeAtom config space' a
                <$> rightArrow config
                <> prettyPrint space b
            BinaryNoParensType op left right ->
                prettyTypeAtom config space' left
                <$> prettyTypeAtom config empty op
                <> prettyPrint space right
            t -> prettyTypeAtom config space' t

-- |
-- Pretty-print a type that *may* be split over multiple lines, if long enough.
--
prettyShortType :: Config -> Doc -> Type -> Doc
prettyShortType config before =
    prettyTypeComplex config empty before empty . insertPlaceholders

-- |
-- Pretty-print a type that *may* be split over multiple lines, if long enough.
--
prettyType :: Config -> Doc -> Doc -> Doc -> Type -> Doc
-- TODO: merge with prettyShortType or clarify documentation.
prettyType config before space' after =
    prettyTypeComplex config before space' after . insertPlaceholders

-- |
-- Pretty-print a white-space-separated list of types that *may* be split
-- over multiple lines.
--
prettyTypes :: Config -> [Type] -> Doc
prettyTypes config types =
    group (line <> vsep (fmap (prettyLongType config empty) types))

-- |
-- Pretty-print a type that *must* be split over multiple lines, if possible.
--
prettyLongType :: Config -> Doc -> Type -> Doc
prettyLongType config@Config{..} before typ = case insertPlaceholders typ of
    PrettyPrintObject row ->
        prettyLongRow config (before <> lbrace) rbrace space row
    row@RCons{} -> prettyLongRow config (before <> lparen) rparen empty row
    app@TypeApp{} -> prettyTypeApp config 1 before app
    ParensInType app@TypeApp{} -> before <> parens (prettyTypeApp config 1 empty app)
    ParensInType typ' ->
        prettyTypeComplex config (before <> discretionarySpace config <> lparen) (flatAlt space empty) (linebreak <> discretionarySpace config <> rparen) typ'
    x -> prettyTypeComplex config empty before empty x

prettyConstraints
    :: Config
    -> Doc
    -> Doc
    -> (Config -> Doc)
    -> [Constraint]
    -> Doc
prettyConstraints config@Config{..} before preArrow arrow = \case
    [] -> empty
    [constraint] ->
        before <> prettyConstraint constraint <> preArrow <> arrow config
    constraints ->
        nest configIndent (prettyEncloseSep (flatAlt empty before <> lparen) rparen (fmap prettyConstraint constraints))
        <> preArrow
        <> arrow config
    where
        prettyConstraint (Constraint class' args _) =
            pretty class'
            <> hcat (fmap (prettyTypeAtom config space) args)

prettyTypeList :: Config -> [(String, Maybe Kind)] -> Doc
prettyTypeList config = spaceSeparatedList . fmap go
    where
        go (s, kind) = case kind of
            Nothing -> text s
            Just kind' ->
                text s <+> doubleColon config <+> prettyKind config kind'

prettyObjectType :: Config -> Doc -> Type -> Doc
prettyObjectType config before =
    prettyRow config (before <> lbrace) rbrace space

prettyRowType :: Config -> Doc -> Type -> Doc
prettyRowType config before =
    prettyRow config (before <> lparen) rparen empty

-- TODO: this name is confusable with prettyRowType.
prettyRow :: Config -> Doc -> Doc -> Doc -> Type -> Doc
prettyRow config open close inside =
    group . prettyLongRow config open close inside

prettyLongRow :: Config -> Doc -> Doc -> Doc -> Type -> Doc
prettyLongRow config@Config{..} open close inside =
    uncurry listToDoc . toList []
    where
        listToDoc types rest = case types of
            [] -> open <> prettyTail rest <> close
            [t] -> start t <> end
            t : ts -> start t <$$> vsep (fmap format ts) <> end
            where
                format typ = comma <+> prettyNameAndType typ
                start typ =
                    open <> flatAlt space inside <> prettyNameAndType typ
                end = tail' <$$> flatAlt empty inside <> close
                tail' = case rest of
                    REmpty -> empty
                    _ -> line <> prettyTail rest
        prettyNameAndType (name, ty)
            | not (isTypeAtom ty) =
                text (prettyPrintObjectKey name)
                <> nest configIndent (prettyTypeComplex config (line <> doubleColon config) space empty ty)
            | otherwise =
                text (prettyPrintObjectKey name)
                <+> doubleColon config
                <> prettyTypeAtom config space ty
        prettyTail = \case
            REmpty -> empty
            other -> pipe <> prettyTypeAtom config space other
        toList tys = \case
            RCons name ty row -> toList ((name, ty) : tys) row
            r -> (reverse tys, r)
