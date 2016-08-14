-- TODO: extra lines get inserted between functions using equational pattern
-- matching.
-- TODO: special characters in strings need to be re-escaped!
-- TODO: sort imported symbols.

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

import Comments (prettyComments)
import Config (Config(..))
import Crash (internalError)
import Kind (prettyKind)
import Names ()
import Pretty (listify, prettyEncloseSep, prettyLongList, prettySingleLineList)
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
                        <> prettyExpr config SoftLine NoIndent expression
                        )
            binders' = hcat (fmap (prettyBinder config Space NoIndent) binders)
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
            ExplicitInstance declarations -> header <> body' declarations
        where
            header =
                text "instance"
                <+> pretty ident
                <+> doubleColon config
                <> prettyConstraints config space space rightFatArrow constraints
                <+> pretty qualified
                <> hcat (fmap (prettyShortType config space) types)
            body' = \case
                [] -> empty
                decls ->
                    space
                    <> text "where"
                    <$> indent configIndent (prettyDeclarations config decls)
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
        <> prettyExpr config Space NoIndent guard
        <+> symbol config
        <> prettyExpr config SoftLine NoIndent expr'
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

prettyExpr :: Config -> WhiteSpace -> Indent -> Expr -> Doc
prettyExpr config@Config{..} before indented = \case
    Literal literal -> prettyLiteralExpr config before literal
    UnaryMinus expr ->
        pretty before <> char '-' <> prettyExpr config Empty indented expr
    BinaryNoParens op left right ->
        pretty before
        <> nest'
            ( prettyExpr config Empty indented left
            </> prettyOp op
            <> prettyExpr config Space NoIndent right
            )
        where
            prettyOp = \case
                Op name -> pretty name
                expr -> tick <> prettyExpr config Empty indented expr <> tick
            nest' = case indented of
                Indent -> nest configIndent
                NoIndent -> id
    Parens expr ->
        pretty before <> parens (prettyExpr config Empty indented expr)
    ObjectGetter s -> pretty before <> underscore <> dot <> text s
    Accessor field expr ->
        prettyExpr config before indented expr <> dot <> pretty field
    ObjectUpdate o ps ->
        line'
        <> group'
            ( before'
            <> prettyExpr config Empty indented o
            <> nest configIndent (formatter rbrace keyValues)
            )
        where
            (line', before') = case (indented, before) of
                (NoIndent, Line) -> (line, empty)
                (NoIndent, _) -> (empty, flatAlt line (pretty before))
                _ -> (pretty before, empty)
            long = any (isExprLong . snd) ps
            group'
                | long = id
                | otherwise = group
            formatter
                | long = prettyLongList lbrace
                | otherwise = prettyEncloseSep (flatAlt empty space <> lbrace)
            keyValues =
                fmap (prettyKeyValue config prettyExpr (space <> equals)) ps
    abstraction@Abs{} -> prettyAbs config before abstraction
    App left right ->
        case indented of
            NoIndent ->
                line'
                <> group' (before' <> nest configIndent (prettyApp config left <> line))
                <> prettyExpr config Empty Indent right
            Indent ->
                pretty before
                <> nest configIndent (group' (prettyApp config left <> line) <> prettyExpr config Empty NoIndent right)
        where
            (line', before')
                | isExprLong right = (line, empty)
                | otherwise = (empty, flatAlt line (pretty before))
            group'
                | isExprLong left = id
                | otherwise = group
    Var qualified -> pretty before <> pretty qualified
    Op qualified -> pretty before <> parens (pretty qualified)
    IfThenElse expr1 expr2 expr3 ->
        pretty before
        <> group'
          ( text "if" <> prettyExpr config Space NoIndent expr1
          <$> text "then" <> nest configIndent (prettyExpr config Space NoIndent expr2)
          <$> text "else" <> nest configIndent (prettyExpr config Space NoIndent expr3)
          )
        where
            group'
                | isExprLong expr1 || isExprLong expr2 || isExprLong expr3 = id
                | otherwise = group
    Constructor qualified -> pretty before <> pretty qualified
    Case exprs caseAlternatives ->
        before'
        <> text "case"
        <> listify (fmap (prettyExpr config Space NoIndent) exprs)
        <+> text "of"
        <$> indent configIndent (vsep (fmap (prettyCaseAlternative config) caseAlternatives))
        where
            before' = case indented of
                NoIndent -> line
                Indent -> pretty before
    TypedValue _ expr typ ->
        prettyExpr config before indented expr
        <+> doubleColon config
        <> prettyShortType config space typ
    Let decls expr ->
        line
        <> nest 4
            ( text "let"
            <+> prettyDeclarations config decls
            )
        <$> text "in"
        <> nest configIndent (prettyExpr config Space NoIndent expr)
    Do doNotationElements ->
        case indented of
            NoIndent ->
                pretty (if before == SoftLine || before == Line then Space else before)
                <> text "do"
                <> elements
            Indent ->
                pretty before
                <> text "do"
                <> nest configIndent elements
        where
            elements =
                hcat (fmap (prettyDoNotationElement config Line) doNotationElements)
    AnonymousArgument -> pretty before <> underscore
    Hole hole -> pretty before <> text ('?' : hole)
    PositionedValue _ comments expr ->
        prettyComments line comments <> prettyExpr config before' indented' expr
        where
            (before', indented') = case comments of
                [] -> (before, indented)
                _ -> (Empty, Indent)
    TypeClassDictionaryConstructorApp _ _ ->
        internalError "TypeClassDictionaryConstructorApp encountered."
    TypeClassDictionary _ _ -> internalError "TypeClassDictionary encountered."
    TypeClassDictionaryAccessor _ _ ->
        internalError "TypeClassDictionaryAccessor encountered."
    SuperClassDictionary _ _ ->
        internalError "SuperClassDictionary encountered."

prettyApp :: Config -> Expr -> Doc
prettyApp config@Config{..} = \case
    App expr1 expr2 ->
        prettyApp config expr1 <> prettyExpr config Line Indent expr2
    expr -> prettyExpr config Empty NoIndent expr

prettyAbs :: Config -> WhiteSpace -> Expr -> Doc
prettyAbs config@Config{..} before expr =
    group' (flatAlt line (pretty before) <> backslash <> go Empty expr)
    where
        go before' = \case
            Abs (Left arg) val ->
                pretty before' <> text (showIdent arg) <> go Space val
            Abs (Right arg) val ->
                prettyBinder config before' NoIndent arg <> go Space val
            val ->
                pretty before'
                <> rightArrow config
                <> nest configIndent (prettyExpr config Line NoIndent val)
        isLong = \case
            Abs _ val -> isLong val
            val -> isExprLong val
        group'
            | isLong expr = id
            | otherwise = group

instance Pretty ImportDeclarationType where
    pretty = \case
        Implicit -> empty
        Explicit refs ->
            space <> prettySingleLineList lparen rparen (fmap pretty refs)
        Hiding refs ->
            space
            <> text "hiding"
            <+> prettySingleLineList lparen rparen (fmap pretty refs)

prettyDoNotationElement :: Config -> WhiteSpace -> DoNotationElement -> Doc
prettyDoNotationElement config@Config{..} before = \case
    DoNotationValue expr -> prettyExpr config before Indent expr
    DoNotationBind binder expr ->
        prettyBinder config before NoIndent binder
        <+> leftArrow config
        <> nest configIndent (prettyExpr config Space NoIndent expr)
    DoNotationLet declarations ->
        pretty before
        <> nest 4
            ( text "let"
            <+> prettyDeclarations config declarations
            )
    PositionedDoNotationElement _ comments element ->
        prettyComments line comments
        <> prettyDoNotationElement config before' element
        where
            before' = case comments of
                [] -> before
                _ -> Empty

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
                <> prettyExpr config SoftLine NoIndent expr
                )
    where
        binders = case caseAlternativeBinders of
            [] -> empty
            x : xs ->
                listify (prettyBinder config Empty NoIndent x : fmap (prettyBinder config Space NoIndent) xs)

prettyLiteral :: Literal a -> Doc
prettyLiteral = \case
    NumericLiteral integerOrDouble -> either pretty pretty integerOrDouble
    StringLiteral s -> dquotes (text s)
    CharLiteral c -> squotes (char c)
    BooleanLiteral b -> text $ if b then "true" else "false"
    _ -> internalError "Unknown literal encountered."

prettyLiteralExpr :: Config -> WhiteSpace -> Literal Expr -> Doc
prettyLiteralExpr config@Config{..} before literal = case literal of
    ArrayLiteral vs ->
        formatter
            lbracket
            rbracket
            (fmap (nest configIndent . prettyExpr config Empty NoIndent) vs)
    ObjectLiteral os ->
        formatter
            lbrace
            rbrace
            (fmap (prettyKeyValue config prettyExpr colon) os)
    _ -> pretty before <> prettyLiteral literal
    where
        formatter symbol
            | isLiteralLong literal = prettyLongList symbol
            | otherwise = prettyEncloseSep (flatAlt empty (pretty before) <> symbol)

prettyKeyValue
    :: Config
    -> (Config -> WhiteSpace -> Indent -> a -> Doc)
    -> Doc
    -> (String, a)
    -> Doc
prettyKeyValue config@Config{..} printer separator (key, val) =
    text (prettyPrintObjectKey key)
    <> separator
    <> nest configIndent (printer config Space NoIndent val)

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

-- TODO: should binders be allowed to be split over multiple lines?
prettyLiteralBinder :: Config -> WhiteSpace -> Literal Binder -> Doc
prettyLiteralBinder config before = \case
    ArrayLiteral vs ->
        prettySingleLineList
            (pretty before <> lbracket)
            rbracket
            (fmap (prettyBinder config Empty NoIndent) vs)
    ObjectLiteral os ->
        prettySingleLineList
            (pretty before <> lbrace)
            rbrace
            (fmap (prettyKeyValue config prettyBinder colon) os)
    literal -> pretty before <> prettyLiteral literal

-- TODO: the Indent parameter isn't currently used.
prettyBinder :: Config -> WhiteSpace -> Indent -> Binder -> Doc
prettyBinder config before indented = \case
    NullBinder -> pretty before <> underscore
    LiteralBinder literalBinder ->
        prettyLiteralBinder config before literalBinder
    VarBinder ident -> pretty before <> pretty ident
    ConstructorBinder constructorName binders ->
        pretty before
        <> pretty constructorName
        <> hcat (fmap (prettyBinder config Space indented) binders)
    OpBinder valueOpName -> pretty before <> pretty valueOpName
    BinaryNoParensBinder opBinder binder1 binder2 ->
        prettyBinder config before indented binder1
        <> prettyBinder config Space indented opBinder
        <> prettyBinder config Space indented binder2
    ParensInBinder binder ->
        pretty before <> parens (prettyBinder config Empty indented binder)
    NamedBinder ident binder ->
        pretty before
        <> pretty ident
        <> at
        <> prettyBinder config Empty indented binder
    PositionedBinder _ comments binder ->
        prettyComments line comments
        <> prettyBinder config before' indented binder
        where
            before' = case comments of
                [] -> before
                _ -> Empty
    TypedBinder typ binder ->
        prettyBinder config before indented binder
        <+> doubleColon config
        <> prettyShortType config space typ

data WhiteSpace
    = Empty
    | Space
    | SoftLine
    | Line
    deriving Eq

instance Pretty WhiteSpace where
    pretty = \case
        Empty -> empty
        Space -> space
        SoftLine -> softline
        Line -> line

data Indent
    = NoIndent
    | Indent
