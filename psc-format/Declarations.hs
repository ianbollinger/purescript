{-# OPTIONS_GHC -fno-warn-orphans #-}

module Declarations where

import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen as PP
import Language.PureScript.AST.Declarations
import Language.PureScript.AST.Operators
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
    pretty (PositionedDeclarationRef _sourceSpan comments declarationRef) =
        comments' <> pretty declarationRef
        where
            comments'
                | null comments = empty
                | otherwise = vsep (fmap pretty comments) <> hardline

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
        pretty ident <> binders' <> body
        where
            body = case expr of
                Left exprs ->
                    empty
                    <$> indent indentationLevel (vcat (fmap printGuardExpr exprs))
                Right expression ->
                    space
                    <> text "="
                    <$> PP.indent indentationLevel (pretty expression)
            binders' = case binders of
                [] -> PP.empty
                _ -> space <> prettyList binders
            printGuardExpr (guard, expr) =
                text "|" <+> pretty guard <+> text "=" <+> pretty expr
    pretty (BindingGroupDeclaration is) = text "BindingGroupDeclaration"
    pretty (ExternDeclaration tdent typ) =
      text "foreign" <+> text "import" <+> pretty tdent <+> text "::" <+> pretty typ
    pretty (ExternDataDeclaration properName kin) =
        text "foreign" <+> text "import" <+> text "data" <+> pretty properName <+> text "::" <+> pretty kin
    pretty (FixityDeclaration fixity) = case fixity of
        Left valueFixity -> pretty valueFixity
        Right typeFixity -> pretty typeFixity
    pretty (ImportDeclaration moduleName importDeclarationType qualifiedModuleName) =
        text "import" <+> pretty moduleName <> importBody
        where
            importBody = case qualifiedModuleName of
                Nothing -> pretty importDeclarationType
                Just qualifiedModuleName' -> pretty importDeclarationType <+> text "as" <+> pretty qualifiedModuleName'
    pretty (TypeClassDeclaration properName a constraints declarations) =
        text "class"
        <+> parens (hsep (fmap pretty constraints))
        <+> text "<="
        <+> pretty properName
        <+> ppTypeList a
        <+> text "where"
        <$> indent indentationLevel (hcat (fmap pretty declarations))
    pretty (TypeInstanceDeclaration ident constraints qualified types body) =
<<<<<<< HEAD
        text "instance"
        <+> pretty ident
        <+> text "::"
        <> constraints'
        <+> pretty qualified
        <+> pretty (head types)
        <+> text "where"
        <$> indent indentationLevel (pretty body)
=======
        case body of
            DerivedInstance -> hardline <> text "derive" <+> header
            ExplicitInstance declarations ->
                hardline
                <> header
                <+> text "where"
                <$> indent indentationLevel (vsep (fmap pretty declarations))
>>>>>>> 1db6ce5... Pretty print derived instances and PositionedDeclarationRefs
        where
            header =
                text "instance"
                <+> pretty ident
                <+> text "::"
                <> constraints'
                <+> pretty qualified
                <+> pretty (head types)
            constraints'
                | null constraints = empty
                | otherwise =
                    space
                    <> parens (hsep (fmap pretty constraints))
                    <+> text "=>"
    pretty (PositionedDeclaration sourceSpan comments declaration) = pretty comments <$> pretty declaration

instance Pretty ValueFixity where
    pretty (ValueFixity fixity (Qualified module' identOrConstructor) opName) =
        pretty fixity <+> pretty name <+> text "as" <+> pretty opName
        where
            name = case identOrConstructor of
                Left ident -> Qualified module' (pretty ident)
                Right constructor -> Qualified module' (pretty constructor)

instance Pretty TypeFixity where
    pretty (TypeFixity fixity typeName opName) =
        pretty fixity <+> pretty typeName <+> text "as" <+> pretty opName

instance Pretty Fixity where
    pretty (Fixity associativity precedence) =
        text (showAssoc associativity) <+> pretty precedence

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
        text "if" <+> pretty expr1 PP.<$> PP.indent indentationLevel (text "then" <+> pretty expr2) PP.<$> PP.indent indentationLevel (text "else" <+> pretty expr3)
    pretty (Constructor qualified) = pretty qualified
    pretty (Case exprs caseAlternatives) =
        text "case"
        <+> listify (fmap pretty exprs)
        <+> text "of"
        <$> PP.indent indentationLevel (vcat (fmap pretty caseAlternatives))
    pretty (TypedValue bool expr typ) = pretty expr <+> text "::" <+> pretty typ
    pretty (Let declarations expr) = pretty expr PP.<$> text "where" PP.<$> vcat (fmap pretty declarations)
    pretty (Do doNotationElements) =
        text "do" PP.<$> PP.indent indentationLevel (vsep (fmap pretty doNotationElements))
    pretty (TypeClassDictionaryConstructorApp qualified expr) = text "TypeClassDictionaryConstructorApp"
    pretty (TypeClassDictionary constraint a) = text "TypeClassDictionary"
    pretty (TypeClassDictionaryAccessor qualified ident) = text "TypeClassDictionaryAccessor"
    pretty (SuperClassDictionary qualified types) = text "SuperClassDictionary"
    pretty AnonymousArgument = text "_"
    pretty (Hole hole) = text "?" <> text hole
    pretty (PositionedValue sourceSpan comments expr) = pretty expr

instance Pretty ImportDeclarationType where
    pretty Implicit = PP.empty
    pretty (Explicit refs) = PP.space <> (prettyTupled . fmap pretty $ refs)
    pretty (Hiding refs) = text "hiding" <+> (tupled . fmap pretty $ refs)

<<<<<<< HEAD
instance Pretty TypeInstanceBody where
    pretty DerivedInstance = PP.empty
    pretty (ExplicitInstance declarations) = vsep . fmap pretty $  declarations

=======
>>>>>>> 1db6ce5... Pretty print derived instances and PositionedDeclarationRefs
instance Pretty DoNotationElement where
    pretty (DoNotationValue expr) = pretty expr
    pretty (DoNotationBind binder expr) = pretty binder <+> text "<-" <+> pretty expr
    pretty (DoNotationLet declarations) =
      text "let" <$> indent indentationLevel (vcat (fmap pretty declarations))
    pretty (PositionedDoNotationElement sourceSpan comments doNotationElement) = text "PositionedDoNotationElement"

instance Pretty CaseAlternative where
    pretty (CaseAlternative caseAlternativeBinders caseAlternativeResult) =
        case caseAlternativeResult of
            Left exprs ->
                binders
                <$> indent indentationLevel (vcat (fmap printGuardExpr exprs))
            Right expr ->
                binders <+> text "->" <+> pretty expr
        where
            binders = listify (fmap pretty caseAlternativeBinders)
            printGuardExpr (guard, expr) =
                text "|" <+> pretty guard <+> text "->" <+> pretty expr
