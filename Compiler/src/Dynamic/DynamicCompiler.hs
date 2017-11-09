{- This module implements a very crude and simple bytecode compiler
   that outputs a bytecode similar to dynamic languages
   - there are no checks for existence of identifiers or types
   - there are original variable names in the "bytecode" which is a simple text file

   VM assumptions
   For simplicity I assume the VM has infinite amount of registers

   Bytecode ref:
   load reg name- loads variable to register `reg`
-}
module Dynamic.DynamicCompiler where

import Parser

compileDef :: Definition -> String
compileDef def = show def

compile :: [Definition] -> String
compile = foldr ((\x y -> x ++ "\n" ++ y) . compileDef) ""
