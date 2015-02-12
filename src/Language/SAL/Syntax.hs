{-|
Module      : Language.SAL.Syntax
Description : Data types for SAL Syntax
Copyright   : (c) Galois Inc, 2015
                  Benjamin F Jones, 2015
License     : MIT
Maintainer  : bjones@galois.com
Stability   : experimental
Portability : Yes

Haskell encoding of the syntax presented in
http://sal.csl.sri.com/doc/language-report.pdf
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

#define DERIVE deriving (Eq, Ord, Show, Typeable, Data)

module Language.SAL.Syntax (
  -- * Types
    TypeDef(..)
  , Type(..)
  , BasicType(..)
  , VarDecl(..)
  , Bound(..)
  , ScalarType(..)
  , DataType(..)
  , Constructor(..)
  , VarType(..)
  , IndexType(..)
  , IndexVarDecl(..)
  , QualifiedName(..)
  -- * Expressions
  , Expr(..)
  , Update(..)
  , UpdatePos(..)
  , Quantifier(..)
  , LetDecl(..)
  , RecordEntry(..)
  , ThenRest(..)
  , ElsIf(..)
  -- * Modules
  , Module(..)
  , ModulePred(..)
  )
where

import Data.Char (chr)
import Data.List (union, (\\))
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.List.NonEmpty (NonEmpty)


------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Identifier for a variable, operator, type, module, context, ...
--  @Identifier := Letter {Letter | Digit | ? | _}∗ | {Opchar}+@
type Identifier = String

-- | @Numeral := {Digit}+@
type Numeral = String

-- | SAL Type Definitions
data TypeDef
    = TypeTypeDef   Type
    | ScalarTypeDef ScalarType
    | DataTypeDef   DataType
  DERIVE

-- | SAL Types
data Type
    = TyBasic    BasicType         -- ^ built-in type, e.g. @BOOLEAN@
    | TyName     Name              -- ^ named type, e.g. @mytype@
    | TySubRange Bound      Bound  -- ^ subrange type, e.g. @[1..n]@
    | TySubType  Identifier Type  Expr  -- ^ subset type, e.g. @{ ident : type | expr }@
    | TyArray    IndexType  Type   -- ^ array type, e.g. @ARRAY idx OF type@
    | TyFunction VarType    Type   -- ^ function type, e.g. @[ var -> type ]@
    | TyRecord   VarDecls          -- ^ record type, e.g. @[# {Identifier : Type}+, #]@
    | TyState    Module         -- ^ module state type, e.g. @MyModule . STATE@
  DERIVE

-- | Basic built-in mathematical types
data BasicType
    = BOOLEAN
    | REAL
    | INTEGER
    | NZINTEGER  -- ^ non-zero integers
    | NATURAL    -- ^ positive integers
    | NZREAL     -- ^ non-zero real numbers
  DERIVE

-- | A type name
type Name = Identifier

-- | Variable declaration of the form: @Identifier : Type@
data VarDecl = VarDecl Identifier Type
  DERIVE

-- | Comma separated variable declarations
type VarDecls = NonEmpty VarDecl

-- | A Bound in a sub-range expression
data Bound
    = Unbounded   -- ^ represents +/- inf depending on context, render as @_@
    | Bound Expr  -- ^ an expression representing a finite bound, render as @Expr@
  DERIVE

-- | Scalar type: @{{Identifier}+, }@
data ScalarType = ScalarType (NonEmpty Identifier)
  DERIVE

-- | Algebraic data type: @DATATYPE Constructors END@
data DataType    = DataType (NonEmpty Constructor) DERIVE
-- | Data type constructors
data Constructor = Constructor Identifier (Maybe VarDecls) DERIVE

-- | Variable type declaration: @[identifier :] type@
data VarType = VarType (Maybe Identifier) Type DERIVE

-- | IndexType is really a subtype of Type:
--
-- > data IndexType = INTEGER | SubRange | ScalarTypeName
data IndexType = IndexType Type DERIVE

-- | Index variable declaration: @Identifier : IndexType@
data IndexVarDecl = IndexVarDecl Identifier IndexType DERIVE

-- | Name of the form: @Identifier[ {ActualParameters} ]!Identifier@
data QualifiedName = QualifiedName Identifier ActualParameters Identifier
  DERIVE


------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

-- | SAL Expression type
data Expr
    = NameExpr Name                    -- ^ named expresssion
    | QualifiedNameExpr QualifiedName  -- ^ qualified named expression
    | NextVar Identifier               -- ^ transition variable: @var'@
    | Numeral Integer
    | App Expr Argument                -- ^ function application
    | InfixApp Expr Identifier Expr    -- ^ infix function application
    | ArraySelec Expr Expr             -- ^ array selection: @Expr[Expr]@
    | RecordSelec Expr Identifier      -- ^ record selection: @Expr.Identifier@
    | TupleSelec Expr Numeral          -- ^ tuple selection: @Expr.Numeral@
    | UpdateExpr Expr Update           -- ^ update expression: @Expr WITH Update@
    | Lambda VarDecls Expr             -- ^ lambda: @LAMBDA (VarDecls) : Expr@
    | QuantifiedExpr Quantifier VarDecls Expr  -- ^ @Quantifier (VarDecls) : Expr@
    | LetExpr (NonEmpty LetDecl) Expr  -- ^ let binding: @LET LetDeclarations IN Expr@
    | SetExpr (Either SetPredExpr SetListExpr)  -- ^ set comprehension: @{ id : ty | expr}@ or,
                                                -- @{ expr1, expr2, ... }@
    | ArrayLit IndexVarDecl Expr       -- ^ array literal: @[[IndexVarDecl] Expr]@
    | RecordLit (NonEmpty RecordEntry) -- ^ record literal: @(# {RecordEntry}+, #)@
    | TupleLit Argument                -- ^ tuple literal
    | Conditional Expr ThenRest        -- ^ conditional: @IF Expr ThenRest@
    | GroupedExpr Expr                 -- ^ expression grouping: @( Expr )@
    | StatePred Module ModulePred      -- ^ module predicate: @Module . ( INIT | TRANS )@
  DERIVE

-- | Comma separated list of expressions
type Argument = [Expr]

-- | Update expression of the form: @UpdatePosition := Expr@
data Update = Update (NonEmpty UpdatePos) Expr
  DERIVE

-- | Elements which may appear in sequence in the 'UpdatePosition' of an 'Update'
-- expression: @{'Argument' | ['Expr'] | .'Identifier' | .'Numeral'}+@
data UpdatePos
    = ArgUpdate Argument      -- @Expr1, Expr2, ...@
    | ExprUpdate Expr         -- @[Expr]@
    | IdentUpdate Identifier  -- @.Identifier@
    | NumUpdate Numeral       -- @.Numeral@
  DERIVE

-- | Quantifier keyword
data Quantifier = FORALL | EXISTS
  DERIVE

-- | Let declaration: @{Identifier : Type = Expr}+,@
data LetDecl = LetDecl Identifier Type Expr
  DERIVE

type SetPredExpr = (Identifier, Type, Expr)  -- ^ @{ Identifier : Type | Expr }@
type SetListExpr = NonEmpty Expr             -- ^ @{ expr1, expr2, ... }@

-- | Record entry: @Identifier := Expr@
data RecordEntry = RecordEntry Identifier Expr
  DERIVE

-- | Continued conditional: @THEN 'Expr' [ 'ElsIf' ] ELSE 'Expr' ENDIF@
data ThenRest = ThenRest Expr [ElsIf] Expr
  DERIVE

-- | More continued conditional: @ELSIF Expr ThenRest@
data ElsIf = ElsIf Expr ThenRest
  DERIVE

------------------------------------------------------------------------
-- Transitions
------------------------------------------------------------------------

-- TODO


------------------------------------------------------------------------
-- Modules
------------------------------------------------------------------------

-- TODO
data Module = Module
  DERIVE

-- | Module predicate
-- XXX what is this for?
data ModulePred = INIT | TRANS
  DERIVE


------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------

-- TODO
type ActualParameters = ()


------------------------------------------------------------------------
-- Tokens
------------------------------------------------------------------------

-- Define special tokens in the SAL Language

keywordSet :: [String]
keywordSet =
  [ "AND", "ARRAY", "BEGIN", "BOOLEAN", "CLAIM", "CONTEXT", "DATATYPE"
  , "DEFINITION", "ELSE" , "ELSIF", "END", "ENDIF", "EXISTS", "FALSE", "FORALL"
  , "GLOBAL", "IF", "IN", "INITIALIZATION" , "INPUT", "INTEGER", "LAMBDA"
  , "LEMMA", "LET", "LOCAL", "MODULE", "NATURAL", "NOT", "NZINTEGER", "NZREAL"
  , "OBLIGATION", "OF", "OR", "OUTPUT", "REAL", "RENAME", "THEN", "THEOREM"
  , "TO", "TRANSITION", "TRUE", "TYPE", "WITH", "XOR"
  ]

specialSet :: [Char]
specialSet= "()[]{}%,.;:'!#?_"

letterSet :: [Char]
letterSet = ['a'..'z'] ++ ['A'..'Z']

digitSet :: [Char]
digitSet = ['0'..'9']

opCharSet :: [Char]
opCharSet = (map chr [33..126]) \\ nonOp
  where
  nonOp = specialSet ++ letterSet ++ digitSet
