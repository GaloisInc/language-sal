module Language.SAL.Syntax.Token where

import Data.Char (chr)

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
opCharSet = (map chr [33..126]) `minus` specialSet `minus` letterSet `minus` digitSet
