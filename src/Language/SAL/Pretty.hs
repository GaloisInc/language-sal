{-|
Module      : Language.SAL.Pretty
Description : Pretty printer for SAL syntax
Copyright   : (c) Galois Inc, 2015
License     : MIT
Maintainer  : Benjamin F Jones <bjones@galois.com>
Stability   : experimental
Portability : Yes
-}

module Language.SAL.Pretty
  ( -- * Printer class
    Pretty(..)
    -- * Rendering
  , renderSAL
  , renderSALStyle
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Non
import Data.List (intersperse)
import Text.PrettyPrint

import Language.SAL.Syntax


-- | A class for things that can be pretty printed
class Pretty a where
  pretty :: a -> Doc

-- | Render a SAL document in default style
renderSAL :: Pretty a => a -> String
renderSAL = render . pretty

-- | Render SAL with 'Text.PrettyPrint.renderStyle'
renderSALStyle :: Pretty a => Style -> a -> String
renderSALStyle s d = renderStyle s (pretty d)

-- | Set the default indent level
tabStop :: Int
tabStop = 4

------------------------------------------------------------------------
-- SAL Specific Print Helpers
------------------------------------------------------------------------

-- | Maybe print a value depending on a Maybe (stolen from
-- "Language.C.Pretty")
maybeP :: (a -> Doc) -> Maybe a -> Doc
maybeP = maybe empty

-- | Print a non-empty list interspersed with the given separator
neP :: Pretty a => Doc {- ^ separator -} -> NonEmpty a -> Doc
neP = neP' pretty

-- | neP with a specific element renderer
neP' :: (a -> Doc) -> Doc -> NonEmpty a -> Doc
neP' rend s xs = hcat . punctuate s . map rend $ Non.toList xs

-- | Vertical version of 'neP'
vneP :: Pretty a => NonEmpty a -> Doc
vneP xs = vcat . map pretty $ Non.toList xs

-- | Vertical version of 'neP\''
vneP' :: (a -> Doc) -> Doc -> NonEmpty a -> Doc
vneP' rend s xs = vcat . punctuate s . map rend $ Non.toList xs

-- | Print a list interspersed with the given separator
listP :: Pretty a => Doc -> [a] -> Doc
listP s xs = hcat . punctuate s $ map pretty xs

-- | Print an optional list
maybeListP :: Pretty a => Doc -> [a] -> Doc
maybeListP _ [] = empty
maybeListP s xs = listP s xs

-- | Brackets for SAL record syntax
recBrackets :: Doc -> Doc
recBrackets d = text "[#" <+> d <+> text "#]"

-- | BEGIN ... END block
beginEnd :: Doc -> Doc
beginEnd d = text "BEGIN" $+$ ii d $+$ text "END"

-- | Field accessor
dot :: Doc
dot = char '.'

-- | Set comprehension
mid :: Doc
mid = char '|'

-- | Synchronous composition
sync :: Doc
sync = text "||"

-- | Asynchronous composition
async :: Doc
async = text "[]"

-- | Transition
trans :: Doc
trans = text "-->"

-- | Indent
ii :: Doc -> Doc
ii = nest tabStop

------------------------------------------------------------------------
-- Pretty Instances
------------------------------------------------------------------------

instance Pretty Identifier where
  pretty = text . identifier_str
instance Pretty Numeral where
  pretty = integer . numeral_val

instance Pretty TypeDef where
  pretty (TypeDef t)     = pretty t
  pretty (ScalarType ns) = braces $ neP (char ',') ns
  pretty (DataType cs)   = text "DATATYPE" <+> neP (char ',') cs <+> text "END"

instance Pretty Type where
  pretty (TyBasic t)       = pretty t
  pretty (TyName t)        = pretty t
  pretty (TySubRange a b)  = brackets $ pretty a <> text ".." <> pretty b
  pretty (TySubType n t e) = braces $ pretty n <> colon <+> pretty t <+> char '|'
                                  <+> pretty e
  pretty (TyArray i t)     = text "ARRAY" <+> pretty i <+> text "OF" <+> pretty t
  pretty (TyFunction v t)  = brackets $ pretty v <+> text "->" <+> pretty t
  pretty (TyRecord vs)     = recBrackets $ pretty vs
  pretty (TyState m)       = pretty m <> text ".STATE"

instance Pretty BasicType where
  pretty = text . show

instance Pretty VarDecls where
  pretty = neP (char ',') . var_decls
instance Pretty VarDecl where
  pretty (VarDecl i t) = pretty i <> colon <+> pretty t

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
    text "LAMBDA" <+> parens (pretty vs) <> colon <+> pretty e
  pretty (QuantifiedExpr q vs e)    =
    pretty q <+> parens (pretty vs) <> colon <+> pretty e
  pretty (LetExpr ls e)             =
    text "LET" <+> neP comma ls <+> text "IN" <+> pretty e
  pretty (SetExpr (Left (i, t, e))) =
    braces $ pretty i <> colon <+> pretty t <+> mid <+> pretty e
  pretty (SetExpr (Right sl))       = braces $ neP comma sl
  pretty (ArrayLit v e)             = brackets (brackets (pretty v) <+> pretty e)
  pretty (RecordLit rs)             = neP comma rs
  pretty (TupleLit a)               = pretty a
  pretty (Conditional e rest)       = text "IF" <+> pretty e <+> pretty rest
  pretty (GroupedExpr e)            = parens (pretty e)
  pretty (StatePred m p)            = pretty m <> char '.' <> pretty p

instance Pretty Argument where
  pretty (Argument as) = parens $ neP comma as

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
  pretty (LetDecl i t e) = pretty i <> colon <+> pretty t <+> text "=" <+> pretty e

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
  pretty (Definitions ls) = neP semi ls

instance Pretty Definition where
  pretty (DefSimple d) = pretty d
  pretty (DefForall vs ds) =
    parens $ text "FORALL" <+> parens (pretty vs) <> colon <+> pretty ds

instance Pretty GuardedCommand where
  pretty (GuardedCommand g as) = pretty g <+> trans <+> maybeListP semi as

instance Pretty ModuleDeclaration where
  pretty (ModuleDeclaration i mv m) =
    pretty i <> maybeP (brackets . pretty) mv <> colon
    <+> text "MODULE" <+> text "=" $+$ pretty m

instance Pretty Module where
  pretty (BaseModule bs) = beginEnd $ vcat (map pretty bs)
  pretty (ModuleInstance (Left n) es) = pretty n  <> brackets (neP comma es)
  pretty (ModuleInstance (Right qn) es) = pretty qn <> brackets (neP comma es)
  pretty (SynchronousComposition m n) = pretty m <+> sync <+> pretty n
  pretty (AsynchronousComposition m n) = pretty m <+> async <+> pretty n
  pretty (MultiSynchronous i t m) =
    parens $ sync <+> parens (pretty i <> colon <+> pretty t) <> colon <+> pretty m
  pretty (MultiAsynchronous i t m) =
    parens $ sync <+> parens (pretty i <> colon <+> pretty t) <> colon <+> pretty m
  pretty (Hiding ns m) =
    text "LOCAL" <+> neP comma ns <+> text "IN" <+> pretty m
  pretty (NewOutput ns m) =
    text "OUTPUT" <+> neP comma ns <+> text "IN" <+> pretty m
  pretty (Renaming rs m) =
    let rend (a,b) = pretty a <+> text "TO" <+> pretty b
    in text "RENAME" <+> neP' rend comma rs <+> text "IN" <+> pretty m
  pretty (WithModule vds m) = text "WITH" <+> vneP vds <+> pretty m
  pretty (ObserveModule m n) =
    text "OBSERVE" <+> pretty m <+> text "WITH" <+> pretty n
  pretty (ParenModule m) = parens (pretty m)

instance Pretty BaseDeclaration where
  pretty (InputDecl  vs) = text "INPUT"  <+> pretty vs
  pretty (OutputDecl vs) = text "OUTPUT" <+> pretty vs
  pretty (GlobalDecl vs) = text "GLOBAL" <+> pretty vs
  pretty (LocalDecl  vs) = text "LOCAL"  <+> pretty vs
  pretty (DefDecl    ds) = text "DEFINITION" $+$ ii (pretty ds)
  pretty (InitDecl   ds) = text "INITIALIZATION" $+$ ii (neP semi ds)
  pretty (TransDecl  ds) = text "TRANSITION" $+$ ii (neP semi ds)

instance Pretty DefinitionOrCommand where
  pretty (DOCDef d) = pretty d
  pretty (DOCCom cs mc) =
    lbrack
      $+$ (vcat . intersperse async . map (ii . pretty) $ Non.toList cs)
      $$  ii (maybeP pretty mc)
      $+$ rbrack

instance Pretty SomeCommand where
  pretty (NamedCommand mi c) =
    case mi of
      Just n -> pretty n <> colon $+$ ii (pretty c)
      Nothing -> pretty c
  pretty (MultiCommand vs c) = parens $ parens (pretty vs) <> colon <+> pretty c

instance Pretty ElseCommand where
  pretty (ElseCommand mi a) =
      case mi of
        Just n -> pretty n <> colon $+$ ii elseCom
        Nothing -> elseCom
    where
    elseCom = text "ELSE" <+> trans <+> maybeListP semi a

instance Pretty ModulePred where
  pretty = text . show

-- Contexts ------------------------------------------------------------

instance Pretty Context where
  pretty (Context n mp b) =
    pretty n <> maybeP pretty mp <> colon <+> text "CONTEXT" <+>
      equals $+$ pretty b

instance Pretty Parameters where
  pretty (Parameters ne vds) =
    brackets (neP comma ne) <> semi <+> maybeListP comma vds

instance Pretty ContextBody where
  pretty (ContextBody ne) = beginEnd $ vneP' ((<> semi) . pretty) empty ne

instance Pretty Declaration where
  pretty (ConstantDecl n mvs t me) =
    pretty n <> maybeP (parens . pretty) mvs <> colon <+> pretty t <+>
    maybeP ((equals <+>) . pretty) me
  pretty (TypeDecl n mt) =
    pretty n <> colon <+> text "TYPE" <+>
    maybeP ((equals <+>) . pretty) mt
  pretty (AssertionDecl n af ae) =
    pretty n <> colon <+> pretty af <+> equals <+> pretty ae
  pretty (ContextDecl n m ap) =
    pretty n <> colon <+> text "CONTEXT" <+> equals <+> pretty m <+> braces (pretty ap)
  pretty (ModuleDecl d) = pretty d

instance Pretty AssertionForm where
  pretty = text . show

instance Pretty AssertionExpr where
  pretty (ModuleModels m e) = pretty m <+> text "|-" <+> pretty e
  pretty (ModuleImplements m1 m2) = pretty m1 <+> text "IMPLEMENTS" <+> pretty m2
  pretty (PosProp op a1 a2) = pretty op <+> parens (pretty a1 <> comma <> pretty a2)
  pretty (NegProp a) = text "NOT" <+> pretty a
  pretty (QuantifiedAssertion q vds ae) =
    pretty q <+> parens (pretty vds) <> colon <+> pretty ae
  pretty (AssertExpr e) = pretty e

instance Pretty PropOp where
  pretty AND  = text "AND"
  pretty OR   = text "OR"
  pretty IMPL = text "=>"
  pretty IFF  = text "<=>"

instance Pretty ActualParameters where
  pretty (ActualParameters ts es) = listP comma ts <+> listP comma es
