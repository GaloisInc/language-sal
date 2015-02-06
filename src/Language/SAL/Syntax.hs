{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

#define DERIVE deriving (Eq, Ord, Show, Typeable, Data)

module Language.SAL.Syntax where

import Data.Char (chr)
import Data.List (union, (\\))
import Data.Data
import Data.Typeable
import Data.List.NonEmpty (NonEmpty)


------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Identifier for a variable, operator, type, module, context, ...
-- Identifier := Letter {Letter | Digit | ? | _}∗ | {Opchar}+
type Identifier = String

-- | Numeral := {Digit}+
type Numeral = String

-- | SAL Type Definitions
data TypeDef
    = TypeTypeDef   Type
    | ScalarTypeDef ScalarType
    | DataTypeDef   SALDataType
  DERIVE

-- | SAL Types
data Type
    = TyBasic    BasicType         -- e.g. BOOLEAN
    | TyName     Name              -- e.g. mytype
    | TySubRange Bound      Bound  -- [a..b]
    | TySubType  Identifier Type  Expr  -- { ident : type | expr }
    | TyArray    IndexType  Type   -- ARRAY idx OF type
    | TyFunction VarType    Type   -- [ var -> type ]
    | TyRecord   VarDecls          -- [# {Identifier : Type}+, #]
    | TyState    SALModule         -- Module . STATE
  DERIVE

-- | Basic mathematical types
data BasicType
    = BOOLEAN
    | REAL
    | INTEGER
    | NZINTEGER  -- NZ means non-zero
    | NATURAL
    | NZREAL
  DERIVE

-- | A type name
type Name = Identifier

-- | Variable declaration of the form: Identifier : Type
data VarDecl = VarDecl Identifier Type
  DERIVE

type VarDecls = NonEmpty VarDecl -- comma sep

-- | A Bound in a sub-range expression
data Bound
    = Unbounded   -- render as _
    | Bound Expr  -- render as Expr
  DERIVE

-- | {{Identifier}+, }
data ScalarType = ScalarType (NonEmpty Identifier)
  DERIVE

-- | DATATYPE Constructors END
data SALDataType    = SALDataType (NonEmpty Constructor) DERIVE
data Constructor = Constructor Identifier (Maybe VarDecls) DERIVE

-- | Variable type declaration: [identifier :] type
data VarType = VarType (Maybe Identifier) Type DERIVE

-- | IndexType is really a subtype of Type:
-- data IndexType = INTEGER | SubRange | ScalarTypeName
data IndexType = IndexType Type DERIVE

-- | Identifier : IndexType
data IndexVarDecl = IndexVarDecl Identifier IndexType DERIVE

-- | Name of the form: Identifier[ {ActualParameters} ]!Identifier
data QualifiedName = QualifiedName Identifier ActualParameters Identifier
  DERIVE


------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

-- | SAL Expression type
data Expr
    = NameExpr Name
    | QualifiedNameExpr QualifiedName
    | NextVar Identifier           -- var'
    | Numeral Integer
    | App Expr Argument            -- Function Argument
    | InfixApp Expr Identifier Expr
    | ArraySelec Expr Expr         -- Expr[Expr]
    | RecordSelec Expr Identifier  -- Expr.Identifier
    | TupleSelec Expr Numeral      -- Expr.Numeral
    | UpdateExpr Expr Update       -- Expr WITH Update
    | Lambda VarDecls Expr         -- LAMBDA (VarDecls) : Expr
    | QuantifiedExpr Quantifier VarDecls Expr  -- Quantifier (VarDecls) : Expr
    | LetExpr (NonEmpty LetDecl) Expr  -- LET LetDeclarations IN Expr
    | SetExpr (Either SetPredExpr SetListExpr)  -- { id : ty | expr} or,
                                                -- { expr1, expr2, ... }
    | ArrayLit IndexVarDecl Expr   -- [[IndexVarDecl] Expr]
    | RecordLit (NonEmpty RecordEntry)  -- (# {RecordEntry}+, #)
    | TupleLit Argument
    | Conditional Expr ThenRest    -- IF Expr ThenRest
    | GroupedExpr Expr             -- ( Expr )
    | StatePred Module ModulePred  -- Module . ( INIT | TRANS )
  DERIVE

-- | Comma separated list of expressions
type Argument = [Expr]

-- | Update expression of the form: UpdatePosition := Expr
data Update = Update (NonEmpty UpdatePos) Expr
  DERIVE

-- | Elements which may appear in sequence in the UpdatePosition of an Update
-- expression: {Argument | [Expr] | .Identifier | .Numeral}+
data UpdatePos
    = ArgUpdate Argument      -- Expr1, Expr2, ...
    | ExprUpdate Expr         -- [Expr]
    | IdentUpdate Identifier  -- .Identifier
    | NumUpdate Numeral       -- .Numeral
  DERIVE

data Quantifier = FORALL | EXISTS
  DERIVE

-- | Let declaration: {Identifier : Type = Expr}+,
data LetDecl = LetDecl Identifier Type Expr
  DERIVE

type SetPredExpr = (Identifier, Type, Expr)  -- { Identifier : Type | Expr }
type SetListExpr = NonEmpty Expr             -- { expr1, expr2, ... }

-- | Identifier := Expr
data RecordEntry = RecordEntry Identifier Expr
  DERIVE

-- | THEN Expr [ ElsIf ] ELSE Expr ENDIF
data ThenRest = ThenRest Expr [ElsIf] Expr
  DERIVE

-- | ELSIF Expr ThenRest
data ElsIf = ElsIf Expr ThenRest
  DERIVE

-- XXX todo or put in other module
data Module = Module
  DERIVE

data ModulePred = INIT | TRANS
  DERIVE


------------------------------------------------------------------------
-- Transitions
------------------------------------------------------------------------

-- TODO


------------------------------------------------------------------------
-- Modules
------------------------------------------------------------------------

-- TODO
data SALModule = SALModule
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
