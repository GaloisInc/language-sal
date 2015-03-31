{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List.NonEmpty
import Language.SAL

main :: IO ()
main = putStrLn (renderSAL ctx)

ctx :: Context
ctx = Context "salctx" Nothing body
  where
  body = ContextBody (ModuleDecl salmod :| [])

salmod :: ModuleDeclaration
salmod =
  let
    ins  = InputDecl  $ VarDecls (VarDecl "x" (TyBasic INTEGER) :| [])
    outs = OutputDecl $ VarDecls (VarDecl "y" (TyBasic INTEGER) :| [])
    expr = InfixApp (NameExpr "x") "+" (NumLit 1)
    d1   = DefSimple (SimpleDefinition (LhsCurrent "y" [])
                                       (RhsExpr expr))
    defs = DefDecl    $ Definitions (d1 :| [])
  in
    ModuleDeclaration "salmod" Nothing $
      BaseModule
        [ ins
        , outs
        , defs
        ]
