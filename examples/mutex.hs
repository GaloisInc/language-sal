{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List.NonEmpty
import Language.SAL

main :: IO ()
main = putStrLn (renderSAL ctx)

ctx :: Context
ctx = Context "mutex" Nothing body
  where
  body = ContextBody $ fromList [tyDecl, modDecl]
  tyDecl = TypeDecl "PC" (Just tyDef)
  tyDef = ScalarType $ fromList ["sleeping", "trying", "critical"]
  modDecl = ModuleDecl proc

proc :: ModuleDeclaration
proc = ModuleDeclaration "process" modParams $ BaseModule [ins, outs, ini, transBlk]
  where
  pc = "PC" :: Identifier
  modParams = Just (VarDecls (VarDecl "tval" (TyBasic BOOLEAN) :| []))
  ins  = InputDecl . VarDecls . fromList $
    [ decl "pc2" pc
    , decl "x2" BOOLEAN
    ]
  outs = OutputDecl . VarDecls . fromList $
    [ decl "pc1" pc
    , decl "x1"  BOOLEAN
    ]
  ini = InitDecl (DOCDef (DefSimple (lhs "pc1" #= rhs "sleeping")) :| [])

  transBlk = TransDecl (trans :| [])
  trans = DOCCom
            (fromList [trWake, trEnter, trLeave])
            Nothing  -- no ELSE block

  trWake = NamedCommand (Just "wakening") $
    GuardedCommand gWake aWake
  gWake = "pc1" .= "sleeping"
  aWake = [ lhs'  "pc1" #= rhs "trying"
          , lhs' "x1"  #= rhs ("x2" .= "tval")
          ]

  trEnter = NamedCommand (Just "entering_critical") $
    GuardedCommand gEnter aEnter
  gEnter = "pc1" .= "trying" .&& ("pc2" .= "sleeping" .|| "x2" ./= "tval")
  aEnter = [ lhs' "pc1" #= rhs "critical" ]

  trLeave = NamedCommand (Just "leaving_critical") $
    GuardedCommand gLeave aLeave
  gLeave = "pc1" .= "critical"
  aLeave = [ lhs' "pc1" #= rhs "sleeping"
           , lhs' "x1"  #= rhs ("x2" .= "tval")
           ]
