module Main where

import Data.List.NonEmpty
import Language.SAL

main :: IO ()
main = print salmod

salmod :: Module
salmod =
  let
    ins  = InputDecl  $ VarDecl "x" (TyBasic INTEGER) :| []
    outs = OutputDecl $ VarDecl "y" (TyBasic INTEGER) :| []
    expr = InfixApp (NameExpr "x") "+" (Numeral 1)
    d1   = DefSimple (SimpleDefinition (LhsCurrent "y" [])
                                       (RhsExpr expr))

    defs = DefDecl    $ d1 :| []
  in
    BaseModule [ ins
               , outs
               , defs
               ]
