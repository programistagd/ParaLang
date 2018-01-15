module Simplify where
import AST

simplify :: [Fun] -> [Fun]
simplify funs = 
    map id funs -- TODO