{-|
Module      : Language.SAL.Syntax
Description : Data types for SAL Syntax
Copyright   : (c) Galois Inc, 2015
License     : MIT
Maintainer  : Benjamin F Jones <bjones@galois.com>
Stability   : experimental
Portability : Yes

Haskell encoding of the syntax presented in
http://sal.csl.sri.com/doc/language-report.pdf

Note that in the concrete syntax below (things in @typewriter font@) the use of
[]'s and {}'s sometimes mean literal brackets/braces and sometimes mean
_optional_ or _set of_. This is confusing, so we use [_ and {_ where appropriate to
denote a literal bracket or brace.

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Each data type in this module derives at least: Eq, Ord, Show, Typeable, Data
#define DERIVE deriving (Eq, Ord, Show, Typeable, Data)

module Language.SAL.Syntax (
  -- * Types
    Identifier(..)
  , Numeral(..)
  , TypeDef(..)
  , Type(..)
  , BasicType(..)
  , VarDecl(..)
  , VarDecls(..)
  , Bound(..)
  , Constructor(..)
  , VarType(..)
  , IndexType
  , IndexVarDecl
  , QualifiedName(..)
  -- * Expressions
  , Expr(..)
  , Argument(..)
  , Update(..)
  , UpdatePos(..)
  , Quantifier(..)
  , LetDecl(..)
  , RecordEntry(..)
  , ThenRest(..)
  , ElsIf(..)
  -- * Transitions
  , SimpleDefinition(..)
  , Access(..)
  , Lhs(..)
  , RhsDefinition(..)
  , Definition(..)
  , Definitions(..)
  , Guard
  , GuardedCommand(..)
  , ElseCommand(..)
  , Assignments
  -- * Modules
  , ModuleDeclaration(..)
  , Module(..)
  , BaseDeclaration(..)
  , DefinitionOrCommand(..)
  , SomeCommand(..)
  , Renames
  , NewVarDecl
  , ModulePred(..)
  -- * Contexts
  , Context(..)
  , Parameters(..)
  , ContextBody(..)
  , Declaration(..)
  , AssertionForm(..)
  , AssertionExpr(..)
  , PropOp(..)
  , ActualParameters(..)
  -- * Tokens
  , keywordSet
  , specialSet
  , letterSet
  , digitSet
  , opCharSet
  )
where

import Data.Char (chr)
import Data.List ((\\))
import Data.Data (Data)
import Data.String
import Data.Typeable (Typeable)
import Data.List.NonEmpty (NonEmpty)


------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Identifier for a variable, operator, type, module, context, ...
--  @Identifier := Letter {Letter | Digit | ? | _}∗ | {Opchar}+@
newtype Identifier = Identifier { identifier_str :: String }
  DERIVE

instance IsString Identifier where
  fromString = Identifier

-- | @Numeral := {Digit}+@
newtype Numeral = Numeral { numeral_val :: Integer }
  deriving (Eq, Ord, Show, Data, Typeable, Num)

-- | SAL Type Definitions
data TypeDef
    = TypeDef    Type
    -- | @{{Identifier}+, }@
    | ScalarType (NonEmpty Identifier)
    -- | @DATATYPE Constructors END@
    | DataType   (NonEmpty Constructor)
  DERIVE

-- | SAL Types
data Type
    -- | built-in type, e.g. @BOOLEAN@
    = TyBasic    BasicType
    -- | named type, e.g. @mytype@
    | TyName     Name
    -- | subrange type, e.g. @[1..n]@
    | TySubRange Bound      Bound
    -- | subset type, e.g. @{ ident : type | expr }@
    | TySubType  Identifier Type  Expr
    -- | array type, e.g. @ARRAY idx OF type@
    | TyArray    IndexType  Type
    -- | function type, e.g. @[ var -> type ]@
    | TyFunction VarType    Type
    -- | record type, e.g. @[# {Identifier : Type}+, #]@
    | TyRecord   VarDecls
    -- | module state type, e.g. @MyModule . STATE@
    | TyState    Module
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
newtype VarDecls = VarDecls { var_decls :: NonEmpty VarDecl }
  DERIVE

-- | A Bound in a sub-range expression
data Bound
    = Unbounded   -- ^ represents +/- inf depending on context, render as @_@
    | Bound Expr  -- ^ an expression representing a finite bound, render as @Expr@
  DERIVE

-- | Data type constructors: @Identifier[(VarDecls)]@
data Constructor = Constructor Identifier (Maybe VarDecls) DERIVE

-- | Variable type declaration: @[identifier :] type@
data VarType = VarType (Maybe Identifier) Type DERIVE

-- | IndexType is really a subtype of Type:
--
-- > data IndexType = INTEGER | SubRange | ScalarTypeName
--
type IndexType = Type
type IndexVarDecl = VarDecl

-- | Name of the form: @Identifier[ {ActualParameters} ]!Identifier@
data QualifiedName = QualifiedName Identifier (Maybe ActualParameters) Identifier
  DERIVE


------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

-- | SAL Expression type
data Expr
    = NameExpr Name                    -- ^ named expresssion
    | QualifiedNameExpr QualifiedName  -- ^ qualified named expression
    | NextVar Identifier               -- ^ transition variable: @var'@
    | NumLit Numeral                   -- ^ integer literal
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

instance IsString Expr where
  fromString = NameExpr . fromString

-- | 'Argument' is a comma separated list of expressions:
--   @( {Expr}+, )@
newtype Argument = Argument (NonEmpty Expr)
  DERIVE

-- | Update expression of the form: @UpdatePosition := Expr@
data Update = Update UpdatePos Expr
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

-- | Let declaration: @Identifier : Type = Expr@
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

-- | Left hand side of a definition
data Lhs = LhsCurrent Identifier [Access]  -- ^ @Identifier@
         | LhsNext    Identifier [Access]  -- ^ @Identifier'@
  DERIVE

-- | Right hand side of a definition, either deterministic assignment or
-- non-deterministic.
data RhsDefinition = RhsExpr      Expr  -- @= Expr@
                   | RhsSelection Expr  -- @IN Expr@
  DERIVE

-- | Variable access
data Access = ArrayAccess  Expr        -- @[_ Expr _]@
            | RecordAccess Identifier  -- @.Identifier@
            | TupleAccess  Numeral     -- @.Numeral@
  DERIVE

-- | @Lhs RhsDefinition@
data SimpleDefinition = SimpleDefinition Lhs RhsDefinition
  DERIVE

data Definition =
    DefSimple SimpleDefinition      -- @SimpleDefinition@
  | DefForall VarDecls Definitions  -- @(FORALL (VarDecls): Definitions)@
  DERIVE

newtype Definitions = Definitions (NonEmpty Definition)  -- @{Definition}+;@
  DERIVE

data GuardedCommand = GuardedCommand Guard Assignments  -- @Guard --> Assignments@
  DERIVE

type Guard = Expr
type Assignments = [SimpleDefinition]  -- @{SimpleDefinition}*;@ (optional ; at end)


------------------------------------------------------------------------
-- Modules
------------------------------------------------------------------------

-- | Top-level module declaration: @Identifier[VarDecls] : MODULE = Module@
data ModuleDeclaration =
    ModuleDeclaration Identifier (Maybe VarDecls) Module
  DERIVE

-- | SAL Module
data Module =
  -- | @BEGIN BaseDeclarations END@
    BaseModule [BaseDeclaration]
  -- | @{Name|QualifiedName} Name[{Expr}+,]@
  | ModuleInstance (Either Name QualifiedName) (NonEmpty Expr)
  -- | @Module || Module@
  | SynchronousComposition Module Module
  -- | @Module [] Module@
  | AsynchronousComposition Module Module
  -- | @(|| (Identifier : IndexType): Module)@
  | MultiSynchronous  Identifier IndexType Module
  -- | @([] (Identifier : IndexType): Module)@
  | MultiAsynchronous Identifier IndexType Module
  -- | @LOCAL {Identifier}+, IN Module@
  | Hiding (NonEmpty Identifier) Module
  -- | @OUTPUT {Identifier}+, IN Module@
  | NewOutput (NonEmpty Identifier) Module
  -- | @RENAME Renames IN Module@
  | Renaming Renames Module
  -- | @WITH NewVarDecls Module
  | WithModule (NonEmpty NewVarDecl) Module
  -- | @OBSERVE Module WITH Module@
  | ObserveModule Module Module
  -- | @( Module )@
  | ParenModule Module
  DERIVE

data BaseDeclaration =
    InputDecl  VarDecls  -- @INPUT VarDecls@
  | OutputDecl VarDecls  -- @OUTPUT VarDecls@
  | GlobalDecl VarDecls  -- @GLOBAL VarDecls@
  | LocalDecl  VarDecls  -- @LOCAL VarDecls@
  | DefDecl    Definitions  -- @DEFINITION Definitions@
  | InitDecl   (NonEmpty DefinitionOrCommand)  -- @{DOC}+;@ (optional ; at end)
  | TransDecl  (NonEmpty DefinitionOrCommand)  -- @{DOC}+;@ (optional ; at end)
  DERIVE

-- | NewVarDecl should be a subtype of BaseDeclaration:
-- data NewVarDecl =
--    InputDecl
--  | OutputDecl
--  | GlobalDecl
--  DERIVE
type NewVarDecl = BaseDeclaration

data DefinitionOrCommand =
    DOCDef Definition
    -- ^ @Definition@
  | DOCCom (NonEmpty SomeCommand) (Maybe ElseCommand)
    -- ^ @[_ {SomeCommand}+[__] [ [__] ElseCommand ] _]@
  DERIVE

data SomeCommand =
  -- | @[ Identifier : ] GuardedCommand@
    NamedCommand (Maybe Identifier) GuardedCommand
  -- | @([__] (VarDecls): SomeCommand)@
  | MultiCommand VarDecls SomeCommand
  DERIVE

-- | @[ Identifier : ] ELSE --> Assignments@
data ElseCommand = ElseCommand (Maybe Identifier) Assignments
  DERIVE

-- | @{Lhs TO Lhs}+,@
type Renames = NonEmpty (Lhs, Lhs)

-- | Part of a 'StatePred' type Expr
data ModulePred = INIT | TRANS
  DERIVE


------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------

-- | @Identifier [ {Parameters} ] : CONTEXT = ContextBody@
data Context = Context Identifier (Maybe Parameters) ContextBody
  DERIVE

-- | @[ {Identifier}+, : TYPE ] ; {VarDecls}*,
data Parameters = Parameters (NonEmpty Identifier) [VarDecls]
  DERIVE

-- | @BEGIN { Declaration; }+ END@
data ContextBody = ContextBody (NonEmpty Declaration)
  DERIVE

-- | Declaration in a context body
data Declaration =
    ConstantDecl  Identifier (Maybe VarDecls) Type (Maybe Expr)
    -- ^ @Identifier[(VarDecls)] : Type [ = Expr ]@
  | TypeDecl      Identifier (Maybe TypeDef)
    -- ^ @Identifier : TYPE [ = TypeDef ]@
  | AssertionDecl Identifier AssertionForm    AssertionExpr
    -- ^ @Identifier : AssertionForm = AssertionExpr@
  | ContextDecl   Identifier Identifier       ActualParameters
  | ModuleDecl    ModuleDeclaration
  DERIVE

-- | Different classes of assertion
data AssertionForm =
    OBLIGATION
  | CLAIM
  | LEMMA
  | THEOREM
  DERIVE

-- | Assertion Expressions allow properties to be stated.
data AssertionExpr =
    ModuleModels     Module Expr
    -- ^ @Module |- Expr@
  | ModuleImplements Module Module
    -- ^ @Module IMPLEMENTS Module@
  | PosProp PropOp AssertionExpr AssertionExpr
    -- ^ @PropOp ( AssertionExpr, AsserstionExpr)@
  | NegProp AssertionExpr
    -- ^ @NOT AssertionExpr@
  | QuantifiedAssertion Quantifier VarDecls AssertionExpr
    -- ^ @Quantifier ( VarDecls ) : AssertionExpr@
  | AssertExpr Expr
  DERIVE

-- | Propositional operators allowed in 'AssertionExpr'
data PropOp =
    AND   -- ^ @AND@
  | OR    -- ^ @OR@
  | IMPL  -- ^ implication @=>@
  | IFF   -- ^ if and only if @<=>@
  DERIVE

-- | @{Type}*, ; {Expr}*,@
data ActualParameters = ActualParameters [Type] [Expr]
  DERIVE


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

specialSet :: String
specialSet= "()[]{}%,.;:'!#?_"

letterSet :: String
letterSet = ['a'..'z'] ++ ['A'..'Z']

digitSet :: String
digitSet = ['0'..'9']

opCharSet :: String
opCharSet = map chr [33..126] \\ nonOp
  where
  nonOp = specialSet ++ letterSet ++ digitSet
