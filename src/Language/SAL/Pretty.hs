{-|
Module      : Language.SAL.Pretty
Description : Pretty printer for SAL syntax
Copyright   : (c) Galois Inc, 2015
                  Benjamin F Jones, 2015
License     : MIT
Maintainer  : bjones@galois.com
Stability   : experimental
Portability : Yes
-}

module Language.SAL.Pretty
  ( -- * Printer class
    Pretty(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Non
import Text.PrettyPrint

import Language.SAL

-- | A class for things that can be pretty printed
class Pretty a where
  pretty :: a -> Doc

-- | Maybe print a value depending on a Maybe (stolen from
-- "Language.C.Pretty")
maybeP :: (a -> Doc) -> Maybe a -> Doc
maybeP = maybe empty

-- | Print a non-empty list interspersed with the given separator
nonEmptyP :: Pretty a => Doc {- ^ separator -} -> NonEmpty a -> Doc
nonEmptyP s non = hcat . punctuate s . map pretty $ Non.toList non

-- | Print a list interspersed with the given separator
listP :: Pretty a => Doc -> [a] -> Doc
listP s xs = hcat . punctuate s $ map pretty xs

-- | Brackets for SAL record syntax
recBrackets :: Doc -> Doc
recBrackets d = text "[#" <+> d <+> text "#]"

------------------------------------------------------------------------
-- Pretty Instances
------------------------------------------------------------------------

instance Pretty Identifier where
  pretty = text . identifier_str
instance Pretty Numeral where
  pretty = integer . numeral_val

instance Pretty TypeDef where
  pretty (TypeDef t)     = pretty t
  pretty (ScalarType ns) = nonEmptyP (char ',') ns
  pretty (DataType cs)   = text "DATATYPE" <+> nonEmptyP (char ',') cs <+> text "END"

instance Pretty Type where
  pretty (TyBasic t)       = pretty t
  pretty (TyName t)        = pretty t
  pretty (TySubRange a b)  = brackets $ pretty a <> text ".." <> pretty b
  pretty (TySubType n t e) = braces $ pretty n <+> colon <+> pretty t <+> char '|'
                                  <+> pretty e
  pretty (TyArray i t)     = text "ARRAY" <+> pretty i <+> text "OF" <+> pretty t
  pretty (TyFunction v t)  = brackets $ pretty v <+> text "->" <+> pretty t
  pretty (TyRecord vs)     = recBrackets $ pretty vs
  pretty (TyState m)       = pretty m <> text ".STATE"

instance Pretty BasicType where
  pretty = text . show

instance Pretty VarDecls where
  pretty = nonEmptyP (char ',') . var_decls
instance Pretty VarDecl where
  pretty (VarDecl i t) = pretty i <+> colon <+> pretty t

instance Pretty Bound where
  pretty Unbounded = char '_'
  pretty (Bound e) = pretty e

instance Pretty Constructor where
  pretty (Constructor i mv) = pretty i <> maybeP (parens . pretty) mv

instance Pretty VarType where
  pretty (VarType mi t) = maybeP ((<+> text ": ") . pretty) mi <> pretty t

instance Pretty QualifiedName where
  pretty (QualifiedName i ma j) = pretty i <> maybeP (braces . pretty) ma
                               <> char '!' <> pretty j

instance Pretty Expr where
  pretty (NameExpr n) = pretty n
  pretty (QualifiedNameExpr q) = pretty q
  pretty (NextVar n) = pretty n <> char '\''
  pretty (NumLit x) = pretty x
  pretty (App e a) = undefined  -- XXX argument? pretty e <+> pretty a
  pretty (InfixApp e op f) = pretty e <+> pretty op <+> pretty f  -- precedence?
  pretty (ArraySelec e f) = pretty e <> brackets (pretty f)
  pretty (RecordSelec e i) = pretty e <> char '.' <> pretty i
  pretty (TupleSelec e x) = pretty e <> char '.' <> pretty x
  pretty (UpdateExpr e up) = pretty e <+> text "WITH" <+> pretty up
  pretty (Lambda vs e) = text "LAMBDA" <+> parens (pretty vs) <+> colon <+> pretty e
  pretty (QuantifiedExpr q vs e) =
    pretty q <+> parens (pretty vs) <+> colon <+> pretty e
  pretty (LetExpr ls e) =
    text "LET" <+> nonEmptyP comma ls <+> text "IN" <+> pretty e
  pretty (SetExpr (Left (i, t, e))) = undefined -- XXX
  pretty (SetExpr (Right sl)) = braces $ nonEmptyP comma sl
  pretty (ArrayLit v e) = brackets (brackets (pretty v) <+> pretty e)
  pretty (RecordLit rs) = nonEmptyP comma rs
  pretty (TupleLit a) = parens $ listP comma a
  pretty (Conditional e rest) = undefined
  pretty (GroupedExpr e) = parens (pretty e)
  pretty (StatePred m p) = pretty m <> char '.' <> pretty p

instance Pretty Update where
  pretty = undefined
instance Pretty UpdatePos where
  pretty = undefined
instance Pretty Quantifier where
  pretty = undefined
instance Pretty LetDecl where
  pretty = undefined
instance Pretty RecordEntry where
  pretty = undefined
instance Pretty ThenRest where
  pretty = undefined
instance Pretty ElsIf where
  pretty = undefined
instance Pretty Lhs where
  pretty = undefined
instance Pretty Access where
  pretty = undefined
instance Pretty SimpleDefinition where
  pretty = undefined
instance Pretty RhsDefinition where
  pretty = undefined
instance Pretty Definition where
  pretty = undefined
instance Pretty GuardedCommand where
  pretty = undefined
instance Pretty ModuleDeclaration where
  pretty = undefined
instance Pretty Module where
  pretty = undefined
instance Pretty BaseDeclaration where
  pretty = undefined
instance Pretty DefinitionOrCommand where
  pretty = undefined
instance Pretty SomeCommand where
  pretty = undefined
instance Pretty ElseCommand where
  pretty = undefined
instance Pretty ModulePred where
  pretty = undefined
instance Pretty ActualParameters where
  pretty (ActualParameters ts es) = listP comma ts <+> listP comma es
