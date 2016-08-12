{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Declarations
    ( prettyDeclaration
    , prettyDeclarations
    ) where

import Prelude hiding ((<$>))

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Declarations (CaseAlternative(..),
                                             Declaration(..),
                                             DeclarationRef(..),
                                             DoNotationElement(..), Expr(..),
                                             Guard, ImportDeclarationType(..),
                                             TypeFixity(..),
                                             TypeInstanceBody(..),
                                             ValueFixity(..))
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.Operators (Fixity(..), showAssoc)
import Language.PureScript.Environment (showDataDeclType)
import Language.PureScript.Names (runProperName, showIdent, showQualified)
import Language.PureScript.Pretty.Common (prettyPrintObjectKey)

import Comments ()
import Config (Config(..))
import Crash (internalError)
import Kind (prettyKind)
import Names ()
import Pretty (listify, prettyEncloseSep, prettyLongList, prettySingleLineList,
               spaceSeparatedList)
import Symbols (at, doubleColon, leftArrow, leftFatArrow, pipe, rightArrow,
                rightFatArrow, tick, underscore)
import Types (prettyConstraints, prettyLongType, prettyShortType, prettyType,
              prettyTypes, prettyTypeList)

instance Pretty DeclarationRef where
    pretty = \case
        TypeRef properName ns -> pretty properName <> constructors
            where
                constructors =
                    case ns of
                        Nothing -> text "(..)"
                        Just [] -> empty
                        Just names ->
                            prettySingleLineList lparen rparen (fmap pretty names)
        TypeOpRef opName -> text "type" <+> parens (pretty opName)
        ValueRef ident -> pretty ident
        ValueOpRef opName -> parens (pretty opName)
        TypeClassRef properName -> text "class" <+> pretty properName
        ModuleRef moduleName -> text "module" <+> pretty moduleName
        PositionedDeclarationRef _ comments declarationRef ->
            prettyList comments <> pretty declarationRef
        TypeInstanceRef _ -> internalError "TypeInstanceRef encountered."
        ReExportRef _ _ -> internalError "ReExportRef encountered."

prettyDeclaration :: Config -> Declaration -> Doc
prettyDeclaration config@Config{..} = \case
    DataDeclaration dataDeclType properName leftTypes constructors ->
        nest configIndent
            ( text (showDataDeclType dataDeclType)
            <+> pretty properName
            <> prettyTypeList config leftTypes
            <> constructors'
            )
        where
            constructors' = case constructors of
                [] -> empty
                [(n, ts)] ->
                    space <> equals <+> pretty n <> prettyTypes config ts
                x : xs ->
                    empty
                    <$> equals
                    <+> formatConstructor x
                    <$> vsep (fmap (\c -> pipe <+> formatConstructor c) xs)
            formatConstructor (n, ts) =
                nest configIndent (pretty n <> prettyTypes config ts)
    TypeSynonymDeclaration propertyName params typ ->
        nest configIndent
            ( text "type"
            <+> pretty propertyName
            <> prettyTypeList config params
            <+> equals
            <> prettyLongType config (flatAlt line space) typ
            )
    TypeDeclaration ident typ ->
        nest configIndent
            ( pretty ident
            <> prettyType config (line <> doubleColon config) space empty typ
            )
    ValueDeclaration ident _ binders expr ->
        pretty ident <> binders' <> body
        where
            body = case expr of
                Left exprs ->
                    empty
                    <$> prettyGuardExprs config (const equals) exprs
                Right expression ->
                    nest configIndent
                        ( space
                        <> equals
                        </> prettyExpr config expression
                        )
            binders' = spaceSeparatedList (fmap (prettyBinder config) binders)
    ExternDeclaration ident typ ->
        nest configIndent
            ( text "foreign"
            <+> text "import"
            <+> pretty ident
            <> prettyType config (line <> doubleColon config) space empty typ
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
    FixityDeclaration fixity -> either pretty pretty fixity
    ImportDeclaration moduleName importDeclarationType qualifiedModuleName ->
        nest configIndent
            ( text "import"
            <+> pretty moduleName
            <> pretty importDeclarationType
            <> qualified
            )
        where
            qualified = case qualifiedModuleName of
                Nothing -> empty
                Just qualifiedModuleName' ->
                    space
                    <> text "as"
                    <+> pretty qualifiedModuleName'
    TypeClassDeclaration properName a constraints declarations ->
        text "class"
        <> prettyConstraints config space space leftFatArrow constraints
        <+> pretty properName
        <> prettyTypeList config a
        <> body
        where
            body = case declarations of
                [] -> empty
                _ ->
                    space
                    <> text "where"
                    <$> indent configIndent (prettyDeclarations config declarations)
    TypeInstanceDeclaration ident constraints qualified types body ->
        case body of
            DerivedInstance -> text "derive" <+> header
            ExplicitInstance declarations ->
                header
                <+> text "where"
                <$> indent configIndent (prettyDeclarations config declarations)
        where
            header =
                text "instance"
                <+> pretty ident
                <+> doubleColon config
                <> prettyConstraints config space space rightFatArrow constraints
                <+> pretty qualified
                -- TODO: why are we only taking the first type?
                <> prettyShortType config space (head types)
    PositionedDeclaration _ comments declaration ->
        prettyList comments <> prettyDeclaration config declaration
    DataBindingGroupDeclaration _ ->
        internalError "DataBindingGroupDeclaration encountered."
    BindingGroupDeclaration _ ->
        internalError "BindingGroupDeclaration encountered."

prettyGuardExpr :: Config -> (Config -> Doc) -> (Guard, Expr) -> Doc
prettyGuardExpr config@Config{..} symbol (guard, expr') =
    nest configIndent
        ( pipe
        <+> prettyExpr config guard
        <+> symbol config
        </> prettyExpr config expr'
        )

prettyGuardExprs :: Config -> (Config -> Doc) -> [(Guard, Expr)] -> Doc
prettyGuardExprs config@Config{..} symbol =
    indent configIndent . vsep . fmap (prettyGuardExpr config symbol)

prettyDeclarations :: Config -> [Declaration] -> Doc
prettyDeclarations config = vsep . fmap (prettyDeclaration config)

instance Pretty ValueFixity where
    pretty (ValueFixity fixity name opName) =
        pretty fixity <+> text name' <+> text "as" <+> pretty opName
        where
            name' = showQualified (either showIdent runProperName) name

instance Pretty TypeFixity where
    pretty (TypeFixity fixity typeName opName) =
        pretty fixity <+> pretty typeName <+> text "as" <+> pretty opName

instance Pretty Fixity where
    pretty (Fixity associativity precedence) =
        text (showAssoc associativity) <+> pretty precedence

prettyExpr :: Config -> Expr -> Doc
prettyExpr config@Config{..} = \case
    Literal literal -> prettyLiteralExpr config literal
    UnaryMinus expr -> char '-' <> prettyExpr config expr
    BinaryNoParens op left right ->
        prettyExpr config left
        </> prettyOp op
        <+> prettyExpr config right
        where
            prettyOp = \case
                Op name -> pretty name
                expr -> tick <> prettyExpr config expr <> tick
    Parens expr -> parens (prettyExpr config expr)
    ObjectGetter s -> underscore <> dot <> text s
    Accessor field expr -> prettyExpr config expr <> dot <> pretty field
    ObjectUpdate o ps ->
        prettyExpr config o <+> formatter lbrace rbrace keyValues
        where
            formatter
                | any (isExprLong . snd) ps = prettyLongList
                | otherwise = prettyEncloseSep
            keyValues =
                fmap (prettyKeyValue config prettyExpr (space <> equals)) ps
    abstraction@Abs{} -> prettyAbs config abstraction
    app@App{} -> group (prettyApp app)
        where
            prettyApp = \case
                App expr1 expr2 -> prettyApp expr1 <$> prettyExpr config expr2
                expr -> prettyExpr config expr
    Var qualified -> pretty qualified
    Op qualified -> parens (pretty qualified)
    IfThenElse expr1 expr2 expr3 ->
        group
          ( text "if" <+> prettyExpr config expr1
          <$> text "then" <+> prettyExpr config expr2
          <$> text "else" <+> prettyExpr config expr3
          )
    Constructor qualified -> pretty qualified
    Case exprs caseAlternatives ->
        line
        <> text "case"
        <+> listify (fmap (prettyExpr config) exprs)
        <+> text "of"
        <$> indent configIndent
            (vsep (fmap (prettyCaseAlternative config) caseAlternatives))
    TypedValue _ expr typ ->
        prettyExpr config expr
        <+> doubleColon config
        <> prettyShortType config space typ
    Let decls expr ->
        line
        <> nest 4
            ( text "let"
            <+> prettyDeclarations config decls
            )
        <$> text "in"
        <+> nest configIndent (prettyExpr config expr)
    Do doNotationElements ->
        text "do"
        <$> vsep (fmap (prettyDoNotationElement config) doNotationElements)
    AnonymousArgument -> underscore
    Hole hole -> text ('?' : hole)
    PositionedValue _ comments expr ->
        prettyList comments <> prettyExpr config expr
    TypeClassDictionaryConstructorApp _ _ ->
        internalError "TypeClassDictionaryConstructorApp encountered."
    TypeClassDictionary _ _ -> internalError "TypeClassDictionary encountered."
    TypeClassDictionaryAccessor _ _ ->
        internalError "TypeClassDictionaryAccessor encountered."
    SuperClassDictionary _ _ ->
        internalError "SuperClassDictionary encountered."

prettyAbs :: Config -> Expr -> Doc
prettyAbs config = (backslash <>) . go
    where
        go = \case
            Abs (Left arg) val -> text (showIdent arg) <+> go val
            Abs (Right arg) val -> prettyBinder config arg <+> go val
            val -> rightArrow config </> prettyExpr config val

instance Pretty ImportDeclarationType where
    pretty = \case
        Implicit -> empty
        Explicit refs ->
            space <> prettySingleLineList lparen rparen (fmap pretty refs)
        Hiding refs ->
            space
            <> text "hiding"
            <+> prettySingleLineList lparen rparen (fmap pretty refs)

prettyDoNotationElement :: Config -> DoNotationElement -> Doc
prettyDoNotationElement config@Config{..} = \case
    DoNotationValue expr -> prettyDoExpr config expr
    DoNotationBind binder expr ->
        prettyBinder config binder
        <+> leftArrow config
        <+> nest configIndent (prettyExpr config expr)
    DoNotationLet declarations ->
        nest 4
            ( text "let"
            <+> prettyDeclarations config declarations
            )
    PositionedDoNotationElement _ comments element ->
        prettyList comments <> prettyDoNotationElement config element

prettyDoExpr :: Config -> Expr -> Doc
prettyDoExpr config@Config{..} = \case
    PositionedValue _ comments expr ->
        prettyList comments <> nest configIndent (prettyDoExpr config expr)
    Case exprs caseAlternatives ->
        text "case"
        <+> listify (fmap (prettyExpr config) exprs)
        <+> text "of"
        <$> vsep (fmap (prettyCaseAlternative config) caseAlternatives)
    expr -> prettyExpr config expr

prettyCaseAlternative :: Config -> CaseAlternative -> Doc
prettyCaseAlternative config@Config{..} CaseAlternative{..} =
    case caseAlternativeResult of
        Left exprs ->
            binders
            <$> prettyGuardExprs config rightArrow exprs
        Right expr ->
            nest configIndent
                ( binders
                <+> rightArrow config
                </> prettyExpr config expr
                )
    where
        binders =
            listify (fmap (prettyBinder config) caseAlternativeBinders)

prettyLiteral :: Literal a -> Doc
prettyLiteral = \case
    NumericLiteral integerOrDouble -> either pretty pretty integerOrDouble
    StringLiteral s -> dquotes (text s)
    CharLiteral c -> squotes (char c)
    BooleanLiteral b -> text $ if b then "true" else "false"
    _ -> internalError "Unknown literal encountered."

prettyLiteralExpr :: Config -> Literal Expr -> Doc
prettyLiteralExpr config@Config{..} literal = case literal of
    ArrayLiteral vs ->
        formatter lbracket rbracket (fmap (prettyExpr config) vs)
    ObjectLiteral os ->
        formatter
            lbrace
            rbrace
            (fmap (prettyKeyValue config prettyExpr colon) os)
    _ -> prettyLiteral literal
    where
        formatter
            | isLiteralLong literal = prettyLongList
            | otherwise = prettyEncloseSep

prettyKeyValue :: Config -> (Config -> a -> Doc) -> Doc -> (String, a) -> Doc
prettyKeyValue config@Config{..} printer separator (key, val) =
    text (prettyPrintObjectKey key)
    <> separator
    <+> nest configIndent (printer config val)

isLiteralLong :: Literal Expr -> Bool
isLiteralLong = \case
    ArrayLiteral vs -> any isExprLong vs
    ObjectLiteral os -> any (isExprLong . snd) os
    _ -> False

isExprLong :: Expr -> Bool
isExprLong = \case
    Literal literal -> isInnerLiteralLong literal
    BinaryNoParens _ left right -> isExprLong left || isExprLong right
    Parens expr -> isExprLong expr
    ObjectUpdate _ _ -> True
    Abs _ _ -> True
    App expr1 expr2 -> isExprLong expr1 || isExprLong expr2
    IfThenElse{} -> True
    Case _ _ -> True
    TypedValue _ expr _ -> isExprLong expr
    Let _ _ -> True
    Do _ -> True
    PositionedValue _ comments expr -> not (null comments) || isExprLong expr
    _ -> False

isInnerLiteralLong :: Literal Expr -> Bool
isInnerLiteralLong = \case
    ArrayLiteral _ -> True
    ObjectLiteral _ -> True
    _ -> False

prettyLiteralBinder :: Config -> Literal Binder -> Doc
prettyLiteralBinder config = \case
    ArrayLiteral vs ->
        prettyEncloseSep lbracket rbracket (fmap (prettyBinder config) vs)
    ObjectLiteral os ->
        prettyEncloseSep
            lbrace
            rbrace
            (fmap (prettyKeyValue config prettyBinder colon) os)
    literal -> prettyLiteral literal

prettyBinder :: Config -> Binder -> Doc
prettyBinder config = \case
    NullBinder -> underscore
    LiteralBinder literalBinder -> prettyLiteralBinder config literalBinder
    VarBinder ident -> pretty ident
    ConstructorBinder constructorName binders ->
        pretty constructorName
        <> spaceSeparatedList (fmap (prettyBinder config) binders)
    OpBinder valueOpName -> pretty valueOpName
    BinaryNoParensBinder opBinder binder1 binder2 ->
        prettyBinder config binder1
        <+> prettyBinder config opBinder
        <+> prettyBinder config binder2
    ParensInBinder binder -> parens (prettyBinder config binder)
    NamedBinder ident binder ->
        pretty ident <> at <> prettyBinder config binder
    PositionedBinder _ comments binder ->
        prettyList comments <> prettyBinder config binder
    TypedBinder typ binder ->
        prettyBinder config binder
        <+> doubleColon config
        <> prettyShortType config space typ
