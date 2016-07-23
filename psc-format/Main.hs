-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

module Main where

import           Prelude                                 hiding (lex)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class               (MonadError (..))
--import Control.Monad.Writer.Strict

import           Data.List                               (intersperse)

--import Options.Applicative ((<>))

import           Text.PrettyPrint.Leijen                 as PP

--import qualified Language.PureScript as P
--import qualified Paths_purescript as Paths

-- https://github.com/purescript/purescript/blob/f6f4de900a5e1705a3356e60b2d8d3589eb7d68d/src/Language/PureScript/Errors.hs#L1209-L1317

--import Language.PureScript.Make
import           Language.PureScript.AST.Binders
import           Language.PureScript.AST.Declarations    (Declaration (..),
                                                          DeclarationRef (..),
                                                          Expr (..), ImportDeclarationType (..),
                                                          Module (Module))
import           Language.PureScript.AST.Declarations    (Declaration (..),
                                                          DeclarationRef (..), DoNotationElement (..),
                                                          Expr (..), ImportDeclarationType (..),
                                                          Module (Module))
import           Language.PureScript.AST.Literals        (Literal (..))
import           Language.PureScript.AST.SourcePos       (SourcePos, SourceSpan)
import           Language.PureScript.Environment         (DataDeclType (..))
import           Language.PureScript.Errors              as P
import           Language.PureScript.Names
import           Language.PureScript.Parser.Declarations
import           Language.PureScript.Pretty.Common       (prettyPrintObjectKey)
import           Language.PureScript.Pretty.Types        (prettyPrintRowWith)
import           Language.PureScript.Pretty.Values       (prettyPrintBinder)
import           Language.PureScript.Types               (Type (..))
indentationLevel :: Int
indentationLevel = 4

vSpace :: Doc
vSpace = PP.line <> PP.line

listify :: [Doc] -> Doc
listify = cat . PP.punctuate (comma <> space)

-- Language.PureScript.Names
instance Pretty (ProperName a) where
    pretty = text . runProperName

-- Language.PureScript.Names
instance Pretty ModuleName where
    pretty (ModuleName moduleName) =
        foldl (PP.<>) PP.empty
            $ intersperse moduleNameSeparator
            $ map pretty moduleName

moduleNameSeparator :: Doc
moduleNameSeparator = text "."

-- Language.PureScript.Names
instance Pretty Ident where
    pretty (Ident i) = text i
    pretty (GenIdent mstring integer) = text "genIdent"

ppQualifiedImport :: Maybe ModuleName -> Doc
ppQualifiedImport (Just m) = PP.space <> text "as" <+> pretty m
ppQualifiedImport Nothing = PP.empty

ppImportDeclarationType :: ImportDeclarationType -> Doc
ppImportDeclarationType Implicit = PP.empty
ppImportDeclarationType (Explicit refs) = PP.space <> (tupled . map pretty $ refs)
ppImportDeclarationType (Hiding refs) = text "hiding" <+> (tupled . map pretty $ refs)

instance Pretty (OpName a) where
    pretty (OpName name) = text name

-- Language.PureScript.AST.Declarations
instance Pretty DeclarationRef where
    pretty (TypeRef properName ns) =
        pretty properName <> constructors
        where
            constructors = case ns of
                Nothing ->
                    PP.empty
                Just [] ->
                    PP.empty
                Just properNames ->
                    space <> listify (map pretty properNames)
    pretty (TypeOpRef (OpName opName)) = text "TypeOpRefs"
    pretty (ValueRef ident) = pretty ident
    pretty (ValueOpRef opName) = parens $ pretty opName
    pretty (TypeClassRef properName) = text "class" <+> pretty properName
    pretty (TypeInstanceRef ident) = text "TypeInstanceRef"
    pretty (ModuleRef moduleName) = pretty moduleName
    pretty (ReExportRef moduleName ref) = text "ReExportRef"
    pretty (PositionedDeclarationRef sourceSpan comments declarationRef) = pretty declarationRef

prettyPrintRowWith :: Char -> Char -> Type -> Doc
prettyPrintRowWith open close = uncurry listToDoc . toList []
    where
        tailToPs :: Type -> Doc
        tailToPs REmpty = PP.empty
        tailToPs other = text "| " <> pretty other

        nameAndTypeToPs :: Char -> String -> Type -> Doc
        nameAndTypeToPs start name ty = text (start : ' ' : name ++ " :: ") <> pretty ty

        listToDoc :: [(String, Type)] -> Type -> Doc
        listToDoc [] REmpty = text [open, close]
        listToDoc [] rest = text [ open, ' ' ] <> tailToPs rest <> text [ ' ', close ]
        listToDoc ts rest = PP.vcat $ zipWith (\(nm, ty) i -> nameAndTypeToPs (if i == 0 then open else ',') nm ty) ts [0 :: Int ..] ++
            [ tailToPs rest, text [close] ]

        toList :: [(String, Type)] -> Type -> ([(String, Type)], Type)
        toList tys (RCons name ty row) = toList ((name, ty):tys) row
        toList tys r = (reverse tys, r)


-- Language.PureScript.AST.Declarations
instance Pretty Declaration where
    pretty (DataDeclaration dataDeclType properName lT cs) =
        text dataLabel <+> pretty properName <+> leftTypes <+> text "=" <+> constructors
        where
            dataLabel =
                case dataDeclType of
                    Data -> "data"
                    Newtype -> "newtype"
            leftTypes = text "leftTypes"
                --hsep . map (\(s, k) -> text s) $ lT
            constructors =
                hsep . intersperse (text "|") . map (\(n, ts) -> pretty n <+> (hsep . map pretty $ ts)) $ cs
    pretty (DataBindingGroupDeclaration declarations) = text "DataBindingGroupDeclaration"
    pretty (TypeSynonymDeclaration propertyName a typ) = text "type" <+> pretty propertyName <+> text "=" <+> pretty typ
    pretty (TypeDeclaration ident typ) = pretty ident <+> text "::" <+> pretty typ
    pretty (ValueDeclaration ident nameKind binders expr) =
        let
            e =
                case expr of
                    Left guards -> text "ValueDeclaration - Guards"
                    Right expression -> pretty expression
        in
            pretty ident <+> text "=" PP.<$> PP.indent indentationLevel e <> vSpace
    pretty (BindingGroupDeclaration is) = text "BindingGroupDeclaration"
    pretty (ExternDeclaration tdent typ) = text "ExternDeclaration"
    pretty (ExternDataDeclaration properName kin) = text "ExternDataDeclaration"
    pretty (FixityDeclaration fixity) = text "FixityDeclaration"
    pretty (ImportDeclaration moduleName importDeclarationType qualifiedModuleName) = text "import" <+> pretty moduleName <> ppImportDeclarationType importDeclarationType <> ppQualifiedImport qualifiedModuleName
    pretty (TypeClassDeclaration properName a constraints declarations) = text "TypeClassDeclaration"
    pretty (TypeInstanceDeclaration ident constraints qualified types typeInstanceBody) = text "instance" <+> pretty ident <+> text "TypeInstanceDeclaration"
    pretty (PositionedDeclaration sourceSpan comments declaration) = pretty declaration


pprintModule :: Module -> Doc
pprintModule (Module sourceSpan comments moduleName declarations _) =
    text "module" <+> pretty moduleName <+> text "where" <> vSpace <> vsep (fmap pretty declarations)

-- https://github.com/purescript/purescript/blob/f6f4de900a5e1705a3356e60b2d8d3589eb7d68d/src/Language/PureScript/Pretty/Types.hs#L28-L39
-- Language.PureScript.Types
instance Pretty Type where
    pretty TypeWildcard{} = text "_"
    pretty (TypeVar var) = text var
    pretty (TypeLevelString s) = text $ show s
    pretty (PrettyPrintObject row) = Main.prettyPrintRowWith '{' '}' row
    pretty (TypeConstructor ctor) = text $ runProperName $ disqualify ctor
    pretty (TUnknown u) = text $ '_' : show u
    pretty (Skolem name s _ _) = text $ name ++ show s
    pretty REmpty = text "()"
    pretty (TypeApp t s) = pretty t <+> pretty s
    pretty row@RCons{} = Main.prettyPrintRowWith '(' ')' row
    pretty (TypeOp op) = text $ showQualified runOpName op
    pretty (BinaryNoParensType op l r) = pretty l <> text " " <> pretty op <> text " " <> pretty r
    pretty _ = text ""

printAbs arg val isFirstAbs =
    case (val, isFirstAbs) of
        (Abs (Left argN) valN, True) ->
            text "\\" <> text (showIdent arg) <+> printAbs argN valN False
        (_, True) ->
            text "\\" <> pretty (showIdent arg) <> text " -> " <> pretty val
        _ ->
            text "" <> pretty (showIdent arg) <> text " -> " <> pretty val

-- Language.PureScript.AST.Declarations
instance Pretty Expr where
    pretty (Literal literal) = pretty literal
    pretty (UnaryMinus expr) = text "-" <> pretty expr
    pretty (BinaryNoParens op left right) = pretty left <+> pretty op <+> pretty right
    pretty (Parens expr) = parens $ pretty expr
    pretty (ObjectGetter s) = text "_." <> text s
    pretty (Accessor field expr) = pretty expr <> dot <> pretty field
    pretty (ObjectUpdate o ps) = pretty o <+> text "{" <+> listify (map (\(key, val) -> text key <+> text "=" <+> pretty val) ps) <+> text "}"
    pretty (Abs (Left arg) val) = printAbs arg val True
    pretty (Abs (Right arg) val) = text "\\" <> text (prettyPrintBinder arg) <> text " -> " <> pretty val
    pretty (App expr1 expr2) = pretty expr1 <+> pretty expr2
    pretty (Var qualified) = pretty qualified
    pretty (Op qualified) = pretty qualified
    pretty (IfThenElse expr1 expr2 expr3) =
        text "if" <+> pretty expr1 PP.<$> text "then" <+> pretty expr2 PP.<$> text "else" <+> pretty expr3
    pretty (Constructor qualified) = pretty qualified
    pretty (Case exprs caseAlternatives) = text "Case"
    pretty (TypedValue bool expr typ) = text "TypedValue"
    pretty (Let declarations expr) = pretty expr PP.<$> text "where" PP.<$> vcat (map pretty declarations)
    pretty (Do doNotationElements) =
        text "do" PP.<$> PP.indent indentationLevel (vsep (map pretty doNotationElements))
    pretty (TypeClassDictionaryConstructorApp qualified expr) = text "TypeClassDictionaryConstructorApp"
    pretty (TypeClassDictionary constraint a) = text "TypeClassDictionary"
    pretty (TypeClassDictionaryAccessor qualified ident) = text "TypeClassDictionaryAccessor"
    pretty (SuperClassDictionary qualified types) = text "SuperClassDictionary"
    pretty AnonymousArgument = text "_"
    pretty (Hole hole) = text hole
    pretty (PositionedValue sourceSpan comments expr) = pretty expr

-- Language.PureScript.Names
instance Pretty a => Pretty (Qualified a) where
    pretty (Qualified mN n) =
        moduleName <> pretty n
        where
            moduleName =
                case mN of
                    Just name -> pretty name <> dot
                    Nothing -> text ""


instance Pretty DoNotationElement where
    pretty (DoNotationValue expr) = pretty expr
    pretty (DoNotationBind binder expr) = pretty binder <+> text "<-" <+> pretty expr
    pretty (DoNotationLet declarations) = text "DoNotationLet"
    pretty (PositionedDoNotationElement sourceSpan comments doNotationElement) = text "PositionedDoNotationElement"

instance Pretty Binder where
    pretty _ = text "Binder"

-- Language.PureScript.AST.Literals
instance Pretty a => Pretty (Literal a) where
    pretty (NumericLiteral id) = text "integer or double"
    pretty (StringLiteral s) = text ("\"" ++ s ++ "\"")
    pretty (CharLiteral c) = text ['\'', c, '\'']
    pretty (BooleanLiteral b) = text $ if b then "true" else "false"
    pretty (ArrayLiteral vs) = PP.list $ map pretty vs
    pretty (ObjectLiteral os) = text "Object {}"

main :: IO ()
main = do
    file <- readFile "/home/joris/Projects/BA-UI/src/Main.purs"

    putStrLn file
    putStrLn "-----------"
    case parseModulesFromFiles id [("Main", file)] of
        Right v -> do
            let [(_, Module _ _ _ declarations _)] = v
            --print declarations
            --putStrLn "------------"
            putStrLn $ displayS (renderCompact $ vsep $ fmap (\(_, m) -> pprintModule m) v) ""
        Left e ->
            putStrLn $ P.prettyPrintMultipleErrors P.defaultPPEOptions e
