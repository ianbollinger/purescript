{-# OPTIONS_GHC -fno-warn-orphans #-}

module Declarations where

import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen as PP
import Language.PureScript.AST.Declarations
import Language.PureScript.Names
import Language.PureScript.Environment (DataDeclType (..))
import Language.PureScript.Pretty.Values (prettyPrintBinder)
import Names
import Types
import Binder
import Data.List (intersperse)
import Config
import Literals
import Pretty
import Comments

printAbs :: Ident -> Expr -> Bool -> Doc
printAbs arg val isFirstAbs =
    case (val, isFirstAbs) of
        (Abs (Left argN) valN, True) ->
            text "\\" <> text (showIdent arg) <+> printAbs argN valN False
        (_, True) ->
            text "\\" <> pretty (showIdent arg) <+> text "->" <+> pretty val
        _ ->
            text "" <> pretty (showIdent arg) <+> text "->" <+> pretty val

instance Pretty DeclarationRef where
    pretty (TypeRef properName ns) =
        pretty properName <> constructors
        where
            constructors = case ns of
                Nothing ->
                    space <> text "(..)"
                Just [] ->
                    PP.empty
                Just properNames ->
                    space <> tupled (fmap pretty properNames)
    pretty (TypeOpRef (OpName opName)) = text "TypeOpRefs"
    pretty (ValueRef ident) = pretty ident
    pretty (ValueOpRef opName) = parens $ pretty opName
    pretty (TypeClassRef properName) = text "class" <+> pretty properName
    pretty (TypeInstanceRef ident) = text "TypeInstanceRef"
    pretty (ModuleRef moduleName) = text "module" <+> pretty moduleName
    pretty (ReExportRef moduleName ref) = text "ReExportRef"
    pretty (PositionedDeclarationRef sourceSpan comments declarationRef) = pretty declarationRef

    prettyList = prettyTupled . fmap pretty

instance Pretty Declaration where
    pretty (DataDeclaration dataDeclType properName lT cs) =
        text dataLabel <+> pretty properName <+> leftTypes <+> text "=" <+> constructors
        where
            dataLabel =
                case dataDeclType of
                    Data -> "data"
                    Newtype -> "newtype"
            leftTypes = ppTypeList lT
            constructors =
                hsep . intersperse (text "|") . fmap (\(n, ts) -> pretty n <+> (hsep . fmap pretty $ ts)) $ cs
    pretty (DataBindingGroupDeclaration declarations) = text "DataBindingGroupDeclaration"
    pretty (TypeSynonymDeclaration propertyName params typ) = text "type" <+> pretty propertyName <+> ppTypeList params <+> text "=" <+> pretty typ
    pretty (TypeDeclaration ident typ) = pretty ident <+> text "::" <+> pretty typ
    pretty (ValueDeclaration ident nameKind binders expr) =
        let
            e =
                case expr of
                    Left guards -> text "ValueDeclaration - Guards"
                    Right expression -> pretty expression
            bs =
                case binders of
                    [] -> PP.empty
                    _ -> space <> prettyList binders
        in
            pretty ident <> bs <+> text "=" <$> PP.indent indentationLevel e
    pretty (BindingGroupDeclaration is) = text "BindingGroupDeclaration"
    pretty (ExternDeclaration tdent typ) =
      text "foreign" <+> text "import" <+> pretty tdent <+> text "::" <+> pretty typ
    pretty (ExternDataDeclaration properName kin) = text "ExternDataDeclaration"
    pretty (FixityDeclaration fixity) = text "FixityDeclaration"
    pretty (ImportDeclaration moduleName importDeclarationType qualifiedModuleName) =
        text "import" <+> pretty moduleName <> importBody
        where
            importBody = case qualifiedModuleName of
                Nothing -> pretty importDeclarationType
                Just qualifiedModuleName' -> pretty importDeclarationType <+> text "as" <+> pretty qualifiedModuleName'
    pretty (TypeClassDeclaration properName a constraints declarations) = text "TypeClassDeclarationsss"
    pretty (TypeInstanceDeclaration ident constraints qualified types typeInstanceBody)
        = text "instance" <+> pretty ident <+> text "::" <+> pretty qualified <+> printTypeConstructors types <+> text "where"
            <$> PP.indent indentationLevel (pretty typeInstanceBody)
    pretty (PositionedDeclaration sourceSpan comments declaration) = pretty comments <$> pretty declaration

instance Pretty Expr where
    pretty (Literal literal) = pretty literal
    pretty (UnaryMinus expr) = text "-" <> pretty expr
    pretty (BinaryNoParens op left right) = pretty left <+> pretty op <+> pretty right
    pretty (Parens expr) = parens $ pretty expr
    pretty (ObjectGetter s) = text "_." <> text s
    pretty (Accessor field expr) = pretty expr <> dot <> pretty field
    pretty (ObjectUpdate o ps) = pretty o <+> text "{" <+> listify (fmap (\(key, val) -> text key <+> text "=" <+> pretty val) ps) <+> text "}"
    pretty (Abs (Left arg) val) = printAbs arg val True
    pretty (Abs (Right arg) val) = text "\\" <> text (prettyPrintBinder arg) <+> text "->" PP.<$> PP.indent indentationLevel (pretty val)
    pretty (App expr1 expr2) = pretty expr1 <+> pretty expr2
    pretty (Var qualified) = pretty qualified
    pretty (Op qualified) = pretty qualified
    pretty (IfThenElse expr1 expr2 expr3) =
        text "if" <+> pretty expr1 PP.<$> text "then" <+> pretty expr2 PP.<$> text "else" <+> pretty expr3
    pretty (Constructor qualified) = pretty qualified
    pretty (Case exprs caseAlternatives) = text "Case"
    pretty (TypedValue bool expr typ) = text "TypedValue"
    pretty (Let declarations expr) = pretty expr PP.<$> text "where" PP.<$> vcat (fmap pretty declarations)
    pretty (Do doNotationElements) =
        text "do" PP.<$> PP.indent indentationLevel (vsep (fmap pretty doNotationElements))
    pretty (TypeClassDictionaryConstructorApp qualified expr) = text "TypeClassDictionaryConstructorApp"
    pretty (TypeClassDictionary constraint a) = text "TypeClassDictionary"
    pretty (TypeClassDictionaryAccessor qualified ident) = text "TypeClassDictionaryAccessor"
    pretty (SuperClassDictionary qualified types) = text "SuperClassDictionary"
    pretty AnonymousArgument = text "_"
    pretty (Hole hole) = text hole
    pretty (PositionedValue sourceSpan comments expr) = pretty expr

instance Pretty ImportDeclarationType where
    pretty Implicit = PP.empty
    pretty (Explicit refs) = PP.space <> (prettyTupled . fmap pretty $ refs)
    pretty (Hiding refs) = text "hiding" <+> (tupled . fmap pretty $ refs)

instance Pretty TypeInstanceBody where
    pretty DerivedInstance = PP.empty
    pretty (ExplicitInstance declarations) = vsep . fmap pretty $  declarations

instance Pretty DoNotationElement where
    pretty (DoNotationValue expr) = pretty expr
    pretty (DoNotationBind binder expr) = pretty binder <+> text "<-" <+> pretty expr
    pretty (DoNotationLet declarations) = text "DoNotationLet"
    pretty (PositionedDoNotationElement sourceSpan comments doNotationElement) = text "PositionedDoNotationElement"
