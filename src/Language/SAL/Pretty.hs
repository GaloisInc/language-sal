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

------------------------------------------------------------------------
-- SAL Specific Print Helpers
------------------------------------------------------------------------

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

-- | Print an optional list
maybeListP :: Doc -> [a] -> Doc
maybeListP _ [] = empty
maybeListP = listP

-- | Brackets for SAL record syntax
recBrackets :: Doc -> Doc
recBrackets d = text "[#" <+> d <+> text "#]"

-- | Field accessor
dot :: Doc
dot = char '.'

-- | Set comprehension
mid :: Doc
mid = char '|'

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
  pretty (NameExpr n)               = pretty n
  pretty (QualifiedNameExpr q)      = pretty q
  pretty (NextVar n)                = pretty n <> char '\''
  pretty (NumLit x)                 = pretty x
  pretty (App e a)                  = pretty e <> pretty a
  pretty (InfixApp e op f)          = pretty e <+> pretty op <+> pretty f
  pretty (ArraySelec e f)           = pretty e <> brackets (pretty f)
  pretty (RecordSelec e i)          = pretty e <> char '.' <> pretty i
  pretty (TupleSelec e x)           = pretty e <> char '.' <> pretty x
  pretty (UpdateExpr e up)          = pretty e <+> text "WITH" <+> pretty up
  pretty (Lambda vs e)              =
    text "LAMBDA" <+> parens (pretty vs) <+> colon <+> pretty e
  pretty (QuantifiedExpr q vs e)    =
    pretty q <+> parens (pretty vs) <+> colon <+> pretty e
  pretty (LetExpr ls e)             =
    text "LET" <+> nonEmptyP comma ls <+> text "IN" <+> pretty e
  pretty (SetExpr (Left (i, t, e))) =
    braces $ pretty i <+> colon <+> pretty t <+> mid <+> pretty e
  pretty (SetExpr (Right sl))       = braces $ nonEmptyP comma sl
  pretty (ArrayLit v e)             = brackets (brackets (pretty v) <+> pretty e)
  pretty (RecordLit rs)             = nonEmptyP comma rs
  pretty (TupleLit a)               = parens $ listP comma a
  pretty (Conditional e rest)       = undefined
  pretty (GroupedExpr e)            = parens (pretty e)
  pretty (StatePred m p)            = pretty m <> char '.' <> pretty p

instance Pretty Update where
  pretty (Update u e) = pretty u <+> text ":=" <+> pretty e

instance Pretty UpdatePos where
  pretty (ArgUpdate a)   = pretty a
  pretty (ExprUpdate e)  = brackets (pretty e)
  pretty (IdentUpdate i) = dot <> pretty i
  pretty (NumUpdate n)   = dot <> pretty n

instance Pretty Quantifier where
  pretty = text . show

instance Pretty LetDecl where
  pretty (LetDecl i t e) = pretty i <+> colon <+> pretty t <+> text "=" pretty e

instance Pretty RecordEntry where
  pretty (RecordEntry i e) = pretty i <+> text ":=" <+> pretty e

instance Pretty ThenRest where
  pretty (ThenRest e els f) = text "THEN" <+> pretty e <+> maybeListP space els
                          <+> text "ELSE" <+> pretty f <+> text "ENDIF"

instance Pretty ElsIf where
  pretty (ElsIf e th) = text "ELSIF" <+> pretty e <+> pretty th

instance Pretty Lhs where
  pretty (LhsCurrent i as) = pretty i <> maybeListP empty as
  pretty (LhsNext    i as) = pretty i <> char '\'' <> maybeListP empty as

instance Pretty Access where
  pretty (ArrayAccess e)  = brackets (pretty e)
  pretty (RecordAccess i) = dot <> pretty i
  pretty (TupleAccess n)  = dot <> pretty n

instance Pretty SimpleDefinition where
  pretty (SimpleDefinition l r) = pretty l <+> pretty r

instance Pretty RhsDefinition where
  pretty (RhsExpr e)      = text "=" <+> pretty e
  pretty (RhsSelection e) = text "IN" <+> pretty e

instance Pretty Definitions where
  pretty (Definitions ls) = nonEmptyP semi ls

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
