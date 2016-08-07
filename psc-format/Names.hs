{-# OPTIONS_GHC -fno-warn-orphans #-}

module Names where

import Prelude

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.Names (Ident, ModuleName, OpName, ProperName,
                                  Qualified(Qualified), runIdent, runModuleName,
                                  runProperName, runOpName)

instance Pretty (ProperName a) where
    pretty = text . runProperName

instance Pretty ModuleName where
    pretty = text . runModuleName

instance Pretty Ident where
    pretty = text . runIdent

instance Pretty (OpName a) where
    pretty = text . runOpName

instance Pretty a => Pretty (Qualified a) where
    pretty (Qualified mN n) = moduleName <> pretty n
        where
            moduleName =
                case mN of
                    Just name -> pretty name <> dot
                    Nothing -> empty
