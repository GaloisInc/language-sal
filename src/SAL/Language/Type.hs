module SAL.Language.Type where


-- | Identifier := Letter {Letter | Digit | ? | _}∗
--               | {Opchar}+
type Identifier = String

-- | Numeral    := {Digit}+
type Numeral    = String

-- | SAL Type Definitions
data TypeDef = TD Type
             | TDScalar ScalarType
             | TDData DataType

-- | SAL Types
data Type = TyBasic BasicType              -- e.g. BOOLEAN
          | TyName Name                    -- e.g. mytype
          | TySubRange SubRange            -- [a..b]
          | TySubType Identifier Type Expression  -- { ident : type | expr }
          | TyArray IndexType Type         -- ARRAY idx OF type
          | TyFunction VarType Type        -- [ var -> type ]
          | TyRecord [(Identifier, Type)]  -- [# {Identifier : Type}+, #]
          | TyState SALModule              -- Module . STATE

-- | Basic mathematical types
data BasicType = BOOLEAN | REAL | INTEGER | NZINTEGER | NATURAL | NZREAL
  deriving (Eq, Show)

type ScalarType = [Identifier]  -- {{Identifier}+, }

type DataType = [(Identifier, Maybe [(Identifier, Type)])]  -- DATATYPE Constructors END

type Name = Identifier

type SubRange = (Bound, Bound)  -- inline?

data Bound = Unbounded         -- render as _
           | Bound Expression  -- render as Expression

type VarType = (Maybe Identifier, Type)  -- [identifier :] type


-- Misc ----------------------------------------------------------------

-- SubType inlined

-- ArrayType inlined

-- How to handle subtyping? Perhaps don't need to since we only construct types, not check
-- them.
-- data IndexType = INTEGER | SubRange | ScalarTypeName

-- XXX do we need?
-- data QualifiedName = undefined  -- Identifier[ {ActualParameters} ]!Identifier


