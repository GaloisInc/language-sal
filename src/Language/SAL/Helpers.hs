{-|
Module      : Language.SAL.Helpers
Description : Helper functions for constructing SAL syntax
Copyright   : (c) Galois Inc, 2015
License     : MIT
Maintainer  : Benjamin F Jones <bjones@galois.com>
Stability   : experimental
Portability : Yes
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.SAL.Helpers
  ( -- * Operators
    (#=)
  , (.=)
  , (./=)
  , (.<)
  , (.>)
  , (.<=)
  , (.>=)
  , (.&&)
  , (.||)
  , (.=>)
  , (.<=>)
  , (.+)
  , (.-)
  , (.*)
  , (./)
    -- * Concise constructors
  , lhs
  , lhs'
  , rhs
  , rhsIn
  , decl
  ) where

import Language.SAL.Syntax


-- Syntax Operators ----------------------------------------------------

infix 1 #=
(#=) :: Lhs -> RhsDefinition -> SimpleDefinition
(#=) = SimpleDefinition


-- Infix Operators -----------------------------------------------------

-- fixity to match Haskell ops
infixl 7 .*, ./
infixl 6 .+, .-

infix 4 .=, ./=, .<, .>, .<=, .>=

infixr 3 .&&
infixr 2 .||

(.=) :: Expr -> Expr -> Expr
x .= y = InfixApp x "=" y

(./=) :: Expr -> Expr -> Expr
x ./= y = InfixApp x "/=" y

(.<) :: Expr -> Expr -> Expr
x .< y = InfixApp x "<" y

(.>) :: Expr -> Expr -> Expr
x .> y = InfixApp x ">" y

(.<=) :: Expr -> Expr -> Expr
x .<= y = InfixApp x "<=" y

(.>=) :: Expr -> Expr -> Expr
x .>= y = InfixApp x ">=" y

(.&&) :: Expr -> Expr -> Expr
x .&& y = InfixApp x "AND" y

(.||) :: Expr -> Expr -> Expr
x .|| y = InfixApp x "||" y

(.=>) :: Expr -> Expr -> Expr
x .=> y = InfixApp x "=>" y

(.<=>) :: Expr -> Expr -> Expr
x .<=> y = InfixApp x "<=>" y

(.+) :: Expr -> Expr -> Expr
x .+ y = InfixApp x "+" y

(.-) :: Expr -> Expr -> Expr
x .- y = InfixApp x "-" y

(.*) :: Expr -> Expr -> Expr
x .* y = InfixApp x "*" y

(./) :: Expr -> Expr -> Expr
x ./ y = InfixApp x "/" y


-- Concise constructors ------------------------------------------------

lhs :: Identifier -> Lhs
lhs v = LhsCurrent v []

lhs' :: Identifier -> Lhs
lhs' v = LhsNext v []

rhs :: Expr -> RhsDefinition
rhs = RhsExpr

rhsIn :: Expr -> RhsDefinition
rhsIn = RhsSelection

decl :: IsType t => Identifier -> t -> VarDecl
decl n t = VarDecl n (toType t)

-- Typeclass Helpers ---------------------------------------------------

-- | Easily embed components in a 'Type'
class IsType a where
  toType :: a -> Type

instance IsType BasicType where
  toType = TyBasic
instance IsType Identifier where
  toType = TyName
instance IsType (Integer,Integer) where
  toType (Numeral->x, Numeral->y) = TySubRange (Bound (NumLit x)) (Bound (NumLit y))
