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
import Language.PureScript.AST.Declarations
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.Operators (Fixity(..), showAssoc)
import Language.PureScript.Environment (DataDeclType(..))
import Language.PureScript.Names (Ident(..), Qualified(..), showIdent)
import Language.PureScript.Pretty.Common (prettyPrintObjectKey)

import Names ()
import Types (prettyConstraints, prettyLongType, prettyLongTypes, prettyType,
              prettyTypeList)
import Config (Config(..))
import Kind (prettyKind)
import Pretty (listify, prettyEncloseSep, prettyLongList, prettyShortList,
               prettyTupled)
import Comments ()
import Symbols (at, doubleColon, leftArrow, leftFatArrow, pipe, rightArrow,
                rightFatArrow, tick, underscore)

prettyAbs :: Config -> Ident -> Expr -> Bool -> Doc
prettyAbs config arg val isFirstAbs =
    case (val, isFirstAbs) of
        (Abs (Left argN) valN, True) ->
            backslash
            <> text (showIdent arg)
            <+> prettyAbs config argN valN False
        (_, True) ->
            backslash
            <> text (showIdent arg)
            <+> rightArrow config
            </> prettyExpr config val
        _ ->
            text (showIdent arg)
            <+> rightArrow config
            </> prettyExpr config val

instance Pretty DeclarationRef where
    pretty = \case
        TypeRef properName ns -> pretty properName <> constructors
            where
                constructors =
                    case ns of
                        Nothing -> text "(..)"
                        Just [] -> empty
                        Just properNames -> tupled (fmap pretty properNames)
        TypeOpRef opName -> text "type" <+> parens (pretty opName)
        ValueRef ident -> pretty ident
        ValueOpRef opName -> parens (pretty opName)
        TypeClassRef properName -> text "class" <+> pretty properName
        TypeInstanceRef _ident -> text "TypeInstanceRef"
        ModuleRef moduleName -> text "module" <+> pretty moduleName
        ReExportRef _moduleName _ref -> text "ReExportRef"
        PositionedDeclarationRef _ comments declarationRef ->
            prettyList comments <> pretty declarationRef

    prettyList = prettyTupled . fmap pretty

prettyDeclaration :: Config -> Declaration -> Doc
prettyDeclaration config@Config{..} = \case
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
                [(n, ts)] ->
                    space <> equals <+> pretty n <> prettyLongTypes config ts
                x : xs ->
                    empty
                    <$> equals
                    <+> formatConstructor x
                    <$> vsep (fmap (\c -> pipe <+> formatConstructor c) xs)
            formatConstructor (n, ts) =
                nest configIndent (pretty n <> prettyLongTypes config ts)
    DataBindingGroupDeclaration _declarations ->
        text "DataBindingGroupDeclaration"
    TypeSynonymDeclaration propertyName params typ ->
        nest configIndent
            ( text "type"
            <+> pretty propertyName
            <> params'
            <+> equals
            <+> prettyLongType config typ
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
                    <$> prettyGuardExprs config (const equals) exprs
                Right expression ->
                    nest configIndent
                        ( space
                        <> equals
                        </> prettyExpr config expression
                        )
            binders' = case binders of
                [] -> empty
                _ -> space <> sep (fmap (prettyBinder config) binders)
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
        <> prettyConstraints config space leftFatArrow constraints
        <+> pretty properName
        <+> prettyTypeList config a
        <+> text "where"
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
                <> prettyConstraints config space rightFatArrow constraints
                <+> pretty qualified
                <+> prettyType config (head types)
    PositionedDeclaration _ comments declaration ->
        prettyList comments <> prettyDeclaration config declaration

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
        prettyExpr config o
        <+> formatter lbrace rbrace (fmap (\(key, val) -> text (prettyPrintObjectKey key) <+> equals <+> nest configIndent (prettyExpr config val)) ps)
        where
            formatter
                | any (isExprLong . snd) ps = prettyLongList
                | otherwise = prettyEncloseSep
    Abs (Left arg) val -> prettyAbs config arg val True
    Abs (Right arg) val ->
        backslash
        <> prettyBinder config arg
        <+> rightArrow config
        <$> indent configIndent (prettyExpr config val)
    App expr1 expr2 -> prettyExpr config expr1 <+> prettyExpr config expr2
    Var qualified -> pretty qualified
    Op qualified -> parens (pretty qualified)
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
    TypedValue _ expr typ ->
        prettyExpr config expr <+> doubleColon config <+> prettyType config typ
    Let decls expr ->
        prettyExpr config expr
        <$> text "where"
        <$> prettyDeclarations config decls
    Do doNotationElements ->
        text "do"
        <$> vsep (fmap (prettyDoNotationElement config) doNotationElements)
    TypeClassDictionaryConstructorApp _qualified _expr ->
        text "TypeClassDictionaryConstructorApp"
    TypeClassDictionary _constraint _a -> text "TypeClassDictionary"
    TypeClassDictionaryAccessor _qualified _ident ->
        text "TypeClassDictionaryAccessor"
    SuperClassDictionary _qualified _types ->
        text "SuperClassDictionary"
    AnonymousArgument -> underscore
    Hole hole -> text ('?' : hole)
    PositionedValue _ comments expr ->
        prettyList comments <> prettyExpr config expr

instance Pretty ImportDeclarationType where
    pretty = \case
        Implicit -> empty
        Explicit refs ->
            space <> prettyShortList lparen rparen (fmap pretty refs)
        Hiding refs ->
            space
            <> text "hiding"
            <+> prettyShortList lparen rparen (fmap pretty refs)

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
    _ -> error "Internal error: unknown literal."

prettyLiteralExpr :: Config -> Literal Expr -> Doc
prettyLiteralExpr config@Config{..} literal = case literal of
    ArrayLiteral vs ->
        formatter lbracket rbracket (fmap (prettyExpr config) vs)
    ObjectLiteral os ->
        formatter
            lbrace
            rbrace
            (fmap (\(key, val) -> text (prettyPrintObjectKey key) <> colon <+> nest configIndent (prettyExpr config val)) os)
    _ -> prettyLiteral literal
    where
        formatter
            | isLiteralLong literal = prettyLongList
            | otherwise = prettyEncloseSep

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
    IfThenElse _ _ _ -> True
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
prettyLiteralBinder config literal = case literal of
    ArrayLiteral vs ->
        prettyEncloseSep lbracket rbracket (fmap (prettyBinder config) vs)
    ObjectLiteral os ->
        prettyEncloseSep
            lbrace
            rbrace
            (fmap (\(key, val) -> text (prettyPrintObjectKey key) <> colon <+> prettyBinder config val) os)
    _ -> prettyLiteral literal

prettyBinder :: Config -> Binder -> Doc
prettyBinder config = \case
    NullBinder -> underscore
    LiteralBinder literalBinder -> prettyLiteralBinder config literalBinder
    VarBinder ident -> pretty ident
    ConstructorBinder constructorName binders -> pretty constructorName <> bs
        where
            bs = case binders of
                [] -> empty
                _ -> space <> hsep (fmap (prettyBinder config) binders)
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
        <+> prettyType config typ
