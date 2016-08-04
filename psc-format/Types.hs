{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import Prelude
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, Doc, cat, text, parens, dot, empty, vcat, sep, (<+>), (<>), (</>), group, fillSep)
import Language.PureScript.Types
import Language.PureScript.Names
import qualified Language.PureScript.Kinds as KK
import Kind ()
import Names ()
import Pretty
import Data.List (intercalate)

-- https://github.com/purescript/purescript/blob/f6f4de900a5e1705a3356e60b2d8d3589eb7d68d/src/Language/PureScript/Pretty/Types.hs#L28-L39
instance Pretty Type where
    pretty (TypeWildcard _) = text "_"
    pretty (TypeVar var) = text var
    pretty (TypeLevelString s) = text $ show s ++ "TypeLevelString"
    pretty (PrettyPrintObject row) = prettyPrintRowWith '{' '}' row
    pretty (TypeConstructor (Qualified moduleName properName)) =
        case moduleName of
            Nothing -> text $ runProperName properName
            Just moduleN -> pretty moduleN <> dot <> text (runProperName properName)
    pretty (TUnknown u) = text $ '_' : show u
    pretty (Skolem name s _ _) = text $ name ++ show s ++ "skolem"
    pretty REmpty = text "()"
    pretty (TypeApp (TypeConstructor (Qualified _ (ProperName "Record"))) s) = prettyPrintRowWith '{' '}' s
    pretty (TypeApp (TypeConstructor (Qualified _ (ProperName "Function"))) s) =
        pretty s </> text "->"
    pretty (TypeApp t s) = pretty t <+> pretty s
    pretty row@RCons{} = prettyPrintRowWith '(' ')' row
    pretty (TypeOp op) = text $ showQualified runOpName op
    pretty (BinaryNoParensType op l r) = pretty l <> text " " <> pretty op <> text " " <> pretty r
    pretty (ParensInType typ) = parens $ pretty typ
    pretty (ForAll s t _) = ppForAll s t []
    pretty (ConstrainedType constraints typ) =
        constraints' </> text "=>" <+> pretty typ
        where
            constraints'
                | length constraints == 1 = pretty (head constraints)
                | otherwise = parens (listify (fmap pretty constraints))
    pretty (KindedType typ kind) = pretty typ <+> text "::" <+> pretty kind
    pretty (PrettyPrintFunction typ1 typ2) = text "PrettyPrintFunction"
    pretty (PrettyPrintForAll xs typ) = text "PrettyPrintForall"

instance Pretty Constraint where
    pretty (Constraint class' args _data) =
        pretty class' <+> sep (fmap pretty args)

ppForAll :: String -> Type -> [String] -> Doc
ppForAll typeVar typ vars =
    case typ of
        ForAll s t _ ->
            ppForAll s t $ typeVar : vars
        _ ->
            text "forall" <+> typeVars <> text "." <+> group (pretty typ)
            where
                typeVars = text . intercalate " " $ typeVar : vars

printTypeConstructors :: [Type] -> Doc
printTypeConstructors as =
    if length as == 1 then
        pretty (printTypeConstructor $ head as)
    else
        text "---!" <> cat (fmap printTypeConstructor as) <> text "!---"

printTypeConstructor :: Type -> Doc
printTypeConstructor (TypeConstructor (Qualified Nothing a)) = text $ runProperName a
printTypeConstructor _  = text "FAILED TO FORMAT TYPE CONSTRUCTOR"

ppTypeList :: [(String, Maybe KK.Kind)] -> Doc
ppTypeList = sep . fmap (\(s, kind) -> text s <> pretty kind)

prettyPrintRowWith :: Char -> Char -> Type -> Doc
prettyPrintRowWith open close = uncurry listToDoc . toList []
    where
        tailToPs :: Type -> Doc
        tailToPs REmpty = empty
        tailToPs other = text "|" <+> pretty other

        nameAndTypeToPs :: Char -> String -> Type -> Doc
        nameAndTypeToPs start name ty = text (start : ' ' : name ++ " :: ") <> pretty ty

        listToDoc :: [(String, Type)] -> Type -> Doc
        listToDoc [] REmpty = text [open, close]
        listToDoc [] rest = text [ open, ' ' ] <> tailToPs rest <> text [ ' ', close ]
        listToDoc ts rest = vcat $ zipWith (\(nm, ty) i -> nameAndTypeToPs (if i == 0 then open else ',') nm ty) ts [0 :: Int ..] ++
            [ tailToPs rest, text [close] ]

        toList :: [(String, Type)] -> Type -> ([(String, Type)], Type)
        toList tys (RCons name ty row) = toList ((name, ty):tys) row
        toList tys r = (reverse tys, r)
