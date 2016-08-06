{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Declarations
    ( prettyDeclaration
    ) where

import Prelude hiding ((<$>))

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Declarations
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.Operators (Fixity(..), showAssoc)
import Language.PureScript.Environment (DataDeclType(..))
import Language.PureScript.Names (Ident(..), OpName(..), Qualified(..), showIdent)
import Language.PureScript.Pretty.Values (prettyPrintBinder)

import Names ()
import Types (prettyConstraint, prettyType, prettyTypeList)
import Config (Config(..))
import Kind (prettyKind)
import Pretty (listify, prettyEncloseSep, prettyTupled)
import Comments ()
import Symbols (at, doubleColon, leftArrow, leftFatArrow, pipe, rightArrow,
                rightFatArrow, tick, underscore)

printAbs :: Config -> Ident -> Expr -> Bool -> Doc
printAbs config arg val isFirstAbs =
    case (val, isFirstAbs) of
        (Abs (Left argN) valN, True) ->
            backslash
            <> text (showIdent arg)
            <+> printAbs config argN valN False
        (_, True) ->
            backslash
            <> text (showIdent arg)
            <+> rightArrow config
            <+> prettyExpr config val
        _ ->
            text (showIdent arg) <+> rightArrow config <+> prettyExpr config val

instance Pretty DeclarationRef where
    pretty (TypeRef properName ns) =
        pretty properName <> constructors
        where
            constructors = case ns of
                Nothing ->
                    text "(..)"
                Just [] ->
                    empty
                Just properNames ->
                    tupled (fmap pretty properNames)
    pretty (TypeOpRef (OpName opName)) = text "type" <+> parens (pretty opName)
    pretty (ValueRef ident) = pretty ident
    pretty (ValueOpRef opName) = parens $ pretty opName
    pretty (TypeClassRef properName) = text "class" <+> pretty properName
    pretty (TypeInstanceRef _ident) = text "TypeInstanceRef"
    pretty (ModuleRef moduleName) = text "module" <+> pretty moduleName
    pretty (ReExportRef _moduleName _ref) = text "ReExportRef"
    pretty (PositionedDeclarationRef _sourceSpan comments declarationRef) =
        comments' <> pretty declarationRef
        where
            comments'
                | null comments = empty
                | otherwise = vsep (fmap pretty comments) <> hardline

    prettyList = prettyTupled . fmap pretty

prettyDeclaration :: Config -> Declaration -> Doc
prettyDeclaration config@Config{..} decl = case decl of
    DataDeclaration dataDeclType properName lT constructors ->
        nest configIndent
            ( text dataLabel
            <+> pretty properName
            <> leftTypes
            <> constructors'
            )
        where
            dataLabel =
                case dataDeclType of
                    Data -> "data"
                    Newtype -> "newtype"
            leftTypes
                | null lT = empty
                | otherwise = space <> prettyTypeList config lT
            constructors' = case constructors of
                [] -> empty
                [x] -> space <> equals <+> formatConstructor x
                x : xs ->
                    empty
                    <$> equals
                    <+> formatConstructor x
                    <$> vsep (fmap (\c -> pipe <+> formatConstructor c) xs)
            formatConstructor (n, ts) = pretty n <> ts'
                where
                    ts'
                        | null ts = empty
                        | otherwise =
                            space <> hsep (fmap (prettyType config) ts)
    DataBindingGroupDeclaration _declarations ->
        text "DataBindingGroupDeclaration"
    TypeSynonymDeclaration propertyName params typ ->
        nest configIndent
            ( text "type"
            <+> pretty propertyName
            <> params'
            <+> equals
            <+> prettyType config typ
            )
        where
            params'
                | null params = empty
                | otherwise = space <> prettyTypeList config params
    TypeDeclaration ident typ ->
        nest configIndent
            (pretty ident </> doubleColon config <+> prettyType config typ)
    ValueDeclaration ident _nameKind binders expr ->
        pretty ident <> binders' <> body
        where
            body = case expr of
                Left exprs ->
                    empty
                    <$> indent configIndent (vsep (fmap printGuardExpr exprs))
                Right expression ->
                    nest configIndent
                      ( space
                      <> equals
                      </> prettyExpr config expression
                      )
            binders' = case binders of
                [] -> empty
                _ -> space <> sep (fmap (prettyBinder config) binders)
            printGuardExpr (guard, expr') =
                nest configIndent
                  ( pipe
                  <+> prettyExpr config guard
                  <+> equals
                  </> prettyExpr config expr'
                  )
    BindingGroupDeclaration _is -> text "BindingGroupDeclaration"
    ExternDeclaration tdent typ ->
        nest configIndent
            ( text "foreign"
            <+> text "import"
            <+> pretty tdent
            </> doubleColon config
            <+> prettyType config typ
            )
    ExternDataDeclaration properName kin ->
        nest configIndent
            ( text "foreign"
            <+> text "import"
            <+> text "data"
            <+> pretty properName
            </> doubleColon config
            <+> prettyKind config kin
            )
    FixityDeclaration fixity ->
        case fixity of
            Left valueFixity -> pretty valueFixity
            Right typeFixity -> pretty typeFixity
    ImportDeclaration moduleName importDeclarationType qualifiedModuleName ->
        text "import" <+> pretty moduleName <> importBody
        where
            importBody = case qualifiedModuleName of
                Nothing -> pretty importDeclarationType
                Just qualifiedModuleName' ->
                    pretty importDeclarationType
                    <+> text "as"
                    <+> pretty qualifiedModuleName'
    TypeClassDeclaration properName a constraints declarations ->
        text "class"
        <> constraints'
        <+> pretty properName
        <+> prettyTypeList config a
        <+> text "where"
        <$> indent configIndent
            (vsep (fmap (prettyDeclaration config) declarations))
        where
            constraints'
                | null constraints = empty
                | length constraints == 1 =
                    space
                    <> prettyConstraint config (head constraints)
                    <+> leftFatArrow config
                | otherwise =
                    space
                    <> parens
                        (listify (fmap (prettyConstraint config) constraints))
                    <+> leftFatArrow config
    TypeInstanceDeclaration ident constraints qualified types body ->
        case body of
            DerivedInstance -> text "derive" <+> header
            ExplicitInstance declarations ->
                header
                <+> text "where"
                <$> indent configIndent
                    (vsep (fmap (prettyDeclaration config) declarations))
        where
            header =
                text "instance"
                <+> pretty ident
                <+> doubleColon config
                <> constraints'
                <+> pretty qualified
                <+> prettyType config (head types)
            constraints'
                | null constraints = empty
                | length constraints == 1 =
                    space
                    <> prettyConstraint config (head constraints)
                    <+> rightFatArrow config
                | otherwise =
                    space
                    <> parens
                        (listify (fmap (prettyConstraint config) constraints))
                    <+> rightFatArrow config
    PositionedDeclaration _sourceSpan comments declaration ->
        comments' <> prettyDeclaration config declaration
        where
            comments'
                | null comments = empty
                | otherwise = vsep (fmap pretty comments) <> hardline

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

prettyExpr :: Config -> Expr -> Doc
prettyExpr config@Config{..} e = case e of
    Literal literal -> prettyLiteralExpr config literal
    UnaryMinus expr -> char '-' <> prettyExpr config expr
    BinaryNoParens op left right ->
        prettyExpr config left
        <+> prettyOp op
        <+> prettyExpr config right
        where
            prettyOp o = case o of
                Op name -> pretty name
                expr -> tick <> prettyExpr config expr <> tick
    Parens expr -> parens (prettyExpr config expr)
    ObjectGetter s -> text ("_." ++ s)
    Accessor field expr -> prettyExpr config expr <> dot <> pretty field
    ObjectUpdate o ps ->
        prettyExpr config o
        <+> lbrace
        <+> listify (fmap (\(key, val) -> text key <+> equals <+> prettyExpr config val) ps)
        <+> rbrace
    Abs (Left arg) val -> printAbs config arg val True
    Abs (Right arg) val ->
        text ('\\' : prettyPrintBinder arg)
        <+> rightArrow config
        <$> indent configIndent (prettyExpr config val)
    App expr1 expr2 -> prettyExpr config expr1 <+> prettyExpr config expr2
    Var qualified -> pretty qualified
    Op qualified -> pretty qualified
    IfThenElse expr1 expr2 expr3 ->
        sep
          [ text "if" <+> prettyExpr config expr1
          , text "then" <+> prettyExpr config expr2
          , text "else" <+> prettyExpr config expr3
          ]
    Constructor qualified -> pretty qualified
    Case exprs caseAlternatives ->
        line
        <> text "case"
        <+> listify (fmap (prettyExpr config) exprs)
        <+> text "of"
        <$> indent configIndent
            (vsep (fmap (prettyCaseAlternative config) caseAlternatives))
    TypedValue _bool expr typ ->
        prettyExpr config expr <+> doubleColon config <+> prettyType config typ
    Let decls expr ->
        prettyExpr config expr
        <$> text "where"
        <$> vsep (fmap (prettyDeclaration config) decls)
    Do doNotationElements ->
        line
        <> text "do"
        <$> indent configIndent
            (vsep (fmap (prettyDoNotationElement config) doNotationElements))
    TypeClassDictionaryConstructorApp _qualified _expr ->
        text "TypeClassDictionaryConstructorApp"
    TypeClassDictionary _constraint _a -> text "TypeClassDictionary"
    TypeClassDictionaryAccessor _qualified _ident ->
        text "TypeClassDictionaryAccessor"
    SuperClassDictionary _qualified _types ->
        text "SuperClassDictionary"
    AnonymousArgument -> underscore
    Hole hole -> text ('?' : hole)
    PositionedValue _sourceSpan comments expr ->
        comments' <> prettyExpr config expr
        where
            comments'
                | null comments = empty
                | otherwise = vsep (fmap pretty comments) <> hardline

instance Pretty ImportDeclarationType where
    pretty typ = case typ of
        Implicit -> empty
        Explicit refs -> space <> prettyTupled (fmap pretty refs)
        Hiding refs -> text "hiding" <+> tupled (fmap pretty refs)

prettyDoNotationElement :: Config -> DoNotationElement -> Doc
prettyDoNotationElement config@Config{..} e = case e of
    DoNotationValue expr -> prettyExpr config expr
    DoNotationBind binder expr ->
        prettyBinder config binder
        <+> leftArrow config
        <+> prettyExpr config expr
    DoNotationLet declarations ->
        nest 4
            ( text "let"
            <+> vsep (fmap (prettyDeclaration config) declarations)
            )
    PositionedDoNotationElement _sourceSpan comments element ->
        comments' <> prettyDoNotationElement config element
        where
            comments'
                | null comments = empty
                | otherwise = vsep (fmap pretty comments) <> hardline

prettyCaseAlternative :: Config -> CaseAlternative -> Doc
prettyCaseAlternative config@Config{..} CaseAlternative{..} =
    case caseAlternativeResult of
        Left exprs ->
            binders
            <$> indent configIndent (vsep (fmap printGuardExpr exprs))
        Right expr ->
            nest configIndent
                ( binders
                <+> rightArrow config
                </> prettyExpr config expr
                )
    where
        binders =
            listify (fmap (prettyBinder config) caseAlternativeBinders)
        printGuardExpr (guard, expr) =
            pipe
            <+> prettyExpr config guard
            <+> rightArrow config
            <+> prettyExpr config expr

prettyLiteral :: Literal a -> Doc
prettyLiteral literal = case literal of
    NumericLiteral integerOrDouble -> case integerOrDouble of
        Left integer' -> pretty integer'
        Right number -> pretty number
    StringLiteral s -> text ("\"" ++ s ++ "\"")
    CharLiteral c -> text ['\'', c, '\'']
    BooleanLiteral b -> text $ if b then "true" else "false"
    _ -> error "Internal error: unknown literal."

prettyLiteralExpr :: Config -> Literal Expr -> Doc
prettyLiteralExpr config literal = case literal of
    ArrayLiteral vs ->
        prettyEncloseSep lbracket rbracket (fmap (prettyExpr config) vs)
    ObjectLiteral os ->
        prettyEncloseSep
            lbrace
            rbrace
            (fmap (\(key, val) -> text key <> colon <+> prettyExpr config val) os)
    _ -> prettyLiteral literal

prettyLiteralBinder :: Config -> Literal Binder -> Doc
prettyLiteralBinder config literal = case literal of
    ArrayLiteral vs ->
        prettyEncloseSep lbracket rbracket (fmap (prettyBinder config) vs)
    ObjectLiteral os ->
        prettyEncloseSep
            lbrace
            rbrace
            (fmap (\(key, val) -> text key <> colon <+> prettyBinder config val) os)
    _ -> prettyLiteral literal

prettyBinder :: Config -> Binder -> Doc
prettyBinder config b = case b of
    NullBinder -> underscore
    LiteralBinder literalBinder -> prettyLiteralBinder config literalBinder
    VarBinder ident -> pretty ident
    ConstructorBinder constructorName binders -> pretty constructorName <> bs
        where
            bs = case binders of
                [] -> empty
                _ -> space <> sep (fmap (prettyBinder config) binders)
    OpBinder _valueOpName -> text "OpBinder"
    BinaryNoParensBinder _binder1 _binder2 _binder3 ->
        text "BinaryNoParensBinder"
    ParensInBinder binder -> parens (prettyBinder config binder)
    NamedBinder ident binder ->
        pretty ident <> at <> prettyBinder config binder
    PositionedBinder _ comments binder ->
        comments' <> prettyBinder config binder
        where
            comments'
                | null comments = empty
                | otherwise = vsep (fmap pretty comments) <> hardline
    TypedBinder typ binder ->
        prettyBinder config binder
        <+> doubleColon config
        <+> prettyType config typ
