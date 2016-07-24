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
import           Data.Maybe
import qualified Language.PureScript                     as P
import           Language.PureScript.AST.Binders
import           Language.PureScript.AST.Declarations    (Declaration (..),
                                                          DeclarationRef (..),
                                                          Expr (..), ImportDeclarationType (..),
                                                          Module (Module))
import           Language.PureScript.AST.Declarations    (Declaration (..),
                                                          DeclarationRef (..), DoNotationElement (..),
                                                          Expr (..), ImportDeclarationType (..),
                                                          Module (Module), TypeInstanceBody (..))
import           Language.PureScript.AST.Literals        (Literal (..))
import           Language.PureScript.AST.SourcePos       (SourcePos, SourceSpan)
import           Language.PureScript.Environment         (DataDeclType (..))
import           Language.PureScript.Errors              as P
import qualified Language.PureScript.Kinds               as KK
import           Language.PureScript.Names
import           Language.PureScript.Parser.Declarations
import           Language.PureScript.Pretty.Common       (prettyPrintObjectKey)
import           Language.PureScript.Pretty.Types        (prettyPrintRowWith)
import           Language.PureScript.Pretty.Values       (prettyPrintBinder)
import           Language.PureScript.Types               (Type (..))
import qualified Options.Applicative                     as Opts

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

ppBinders :: [Binder] -> Doc
ppBinders = sep . map pretty

instance Pretty (OpName a) where
    pretty (OpName name) = text name

-- Language.PureScript.AST.Declarations
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
                    space <> tupled (map pretty properNames)
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
        tailToPs other = text "|" <+> pretty other

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

ppKind :: Maybe KK.Kind -> Doc
ppKind Nothing = PP.empty
ppKind (Just k) =
    let
        sign =
            case k of
                KK.KUnknown a ->
                    show a
                KK.Star -> "*"
                KK.Bang -> "!"
                KK.Row kind -> "ROW KIND"
                KK.FunKind kind1 kind2 ->  "FUNKIND KIND KIND"
                KK.Symbol -> "SYMBOLL"
    in
        text ":" <+> text sign

ppTypeList :: [(String, Maybe KK.Kind)] -> Doc
ppTypeList = cat . map (\(s, kind) -> text s <> ppKind kind)


-- Language.PureScript.AST.Declarations
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
                hsep . intersperse (text "|") . map (\(n, ts) -> pretty n <+> (hsep . map pretty $ ts)) $ cs
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
                    _ -> space <> ppBinders binders
        in
            pretty ident <> bs <+> text "=" PP.<$> PP.indent indentationLevel e <> vSpace
    pretty (BindingGroupDeclaration is) = text "BindingGroupDeclaration"
    pretty (ExternDeclaration tdent typ) = text "ExternDeclaration"
    pretty (ExternDataDeclaration properName kin) = text "ExternDataDeclaration"
    pretty (FixityDeclaration fixity) = text "FixityDeclaration"
    pretty (ImportDeclaration moduleName importDeclarationType qualifiedModuleName) = text "import" <+> pretty moduleName <> ppImportDeclarationType importDeclarationType <> ppQualifiedImport qualifiedModuleName
    pretty (TypeClassDeclaration properName a constraints declarations) = text "TypeClassDeclarationsss"
    pretty (TypeInstanceDeclaration ident constraints qualified types typeInstanceBody)
        = text "instance" <+> pretty ident <+> text "::" <+> pretty qualified <+> printTypeConstructors types <+> text "where"
            PP.<$> PP.indent indentationLevel (pretty typeInstanceBody)
    pretty (PositionedDeclaration sourceSpan comments declaration) = pretty declaration

printTypeConstructors as =
    if length as == 1 then
        pretty (printTypeConstructor $ head as)
    else
        text "---!" <> PP.cat (map printTypeConstructor as) <> text "!---"

printTypeConstructor (TypeConstructor (Qualified Nothing a)) = text $ P.runProperName a
printTypeConstructor _  = text "FAILED TO FORMAT TYPE CONSTRUCTOR"

pprintModule :: Module -> Doc
pprintModule (Module sourceSpan comments moduleName declarations _) =
    text "module" <+> pretty moduleName <+> text "where" <> vSpace <> vsep (fmap pretty declarations)

instance Pretty TypeInstanceBody where
    pretty DerivedInstance = PP.empty
    pretty (ExplicitInstance declarations) = vsep . map pretty $  declarations

-- https://github.com/purescript/purescript/blob/f6f4de900a5e1705a3356e60b2d8d3589eb7d68d/src/Language/PureScript/Pretty/Types.hs#L28-L39
-- Language.PureScript.Types
instance Pretty Type where
    pretty (TypeWildcard _) = text "_"
    pretty (TypeVar var) = text var
    pretty (TypeLevelString s) = text $ show s ++ "TypeLevelString"
    pretty (PrettyPrintObject row) = Main.prettyPrintRowWith '{' '}' row
    pretty (TypeConstructor ctor) = text $ runProperName $ disqualify ctor
    pretty (TUnknown u) = text $ '_' : show u
    pretty (Skolem name s _ _) = text $ name ++ show s ++ "skolem"
    pretty REmpty = text "()"
    pretty (TypeApp (TypeConstructor (Qualified _ (ProperName "Record"))) s) = Main.prettyPrintRowWith '{' '}' s
    pretty (TypeApp (TypeConstructor (Qualified _ (ProperName "Function"))) s) = pretty s <+> text "->"
    pretty (TypeApp t s) = pretty t <+> pretty s
    pretty row@RCons{} = Main.prettyPrintRowWith '(' ')' row
    pretty (TypeOp op) = text $ showQualified runOpName op
    pretty (BinaryNoParensType op l r) = pretty l <> text " " <> pretty op <> text " " <> pretty r
    pretty (ParensInType typ) = parens $ pretty typ
    pretty (ForAll s t _) = text ("forall " ++ s ++ ".") <+> pretty t
    pretty (ConstrainedType constraints typ) = text "ConstrainedType"
    pretty (KindedType typ kind) = pretty "KindedType"
    pretty (PrettyPrintFunction typ1 typ2) = text "PrettyPrintFunction"
    pretty (PrettyPrintForAll xs typ) = text "PrettyPrintForall"

printAbs arg val isFirstAbs =
    case (val, isFirstAbs) of
        (Abs (Left argN) valN, True) ->
            text "\\" <> text (showIdent arg) <+> printAbs argN valN False
        (_, True) ->
            text "\\" <> pretty (showIdent arg) <+> text "->" <+> pretty val
        _ ->
            text "" <> pretty (showIdent arg) <+> text "->" <+> pretty val

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
    pretty (Abs (Right arg) val) = text "\\" <> text (prettyPrintBinder arg) <+> text "->" PP.<$> PP.indent indentationLevel (pretty val)
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
    pretty NullBinder = text "_"
    pretty (LiteralBinder literalBinder) = text "LiteralBinder"
    pretty (VarBinder ident) = pretty ident
    pretty (ConstructorBinder constructorName binders) = pretty constructorName <> bs
        where
            bs = case binders of
                [] -> PP.empty
                _ -> space <> ppBinders binders
    pretty (OpBinder valueOpName) = text "OpBinder"
    pretty (BinaryNoParensBinder binder1 binder2 binder3) = text "BinaryNoParensBinder"
    pretty (ParensInBinder binder) = parens . pretty $ binder
    pretty (NamedBinder ident binder) = text "NamedBinder"
    pretty (PositionedBinder _ comments binder) = pretty binder
    pretty (TypedBinder typ binder) = text "TypedBinder"


-- Language.PureScript.AST.Literals
instance Pretty a => Pretty (Literal a) where
    pretty (NumericLiteral id) = text "integer or double"
    pretty (StringLiteral s) = text ("\"" ++ s ++ "\"")
    pretty (CharLiteral c) = text ['\'', c, '\'']
    pretty (BooleanLiteral b) = text $ if b then "true" else "false"
    pretty (ArrayLiteral vs) = PP.list $ map pretty vs
    pretty (ObjectLiteral os) = text "{" <+> listify (map (\(key, val) -> text key <+> text ":" <+> pretty val) os) <+> text "}"


data Config = Config
  { input  :: String
  , output :: String }

config :: Opts.Parser Config
config = Config
     Opts.<$> Opts.strOption
         ( Opts.long "input"
        Opts.<> Opts.metavar ""
        Opts.<> Opts.help "specify path to input file" )
     Opts.<*> Opts.strOption
         ( Opts.long "output"
        Opts.<> Opts.metavar ""
        Opts.<> Opts.help "specify path to output file" )

runFormatter :: Config -> IO ()
runFormatter (Config i o) = do
    inputFile <- readFile i
    case parseModulesFromFiles id [("Main", inputFile)] of
            Right v -> do
                let [(_, Module _ _ _ declarations _)] = v
                writeFile o $ displayS (renderPretty 0.9 120 $ vsep $ fmap (\(_, m) -> pprintModule m) v) ""
            Left e ->
                putStrLn $ P.prettyPrintMultipleErrors P.defaultPPEOptions e
runFormatter _ = return ()

main :: IO ()
main = Opts.execParser opts >>= runFormatter
  where
    opts = Opts.info (Opts.helper <*> config)
      ( Opts.fullDesc
     Opts.<> Opts.progDesc "run this program to format a purs file. "
     Opts.<> Opts.header "psc-format - format purescript files" )
