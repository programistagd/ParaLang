module Substitution where
import Data.Map (Map)
import qualified Data.Map as Map

import AST

extractVars :: [Statement] -> Map String Expression
extractVars stmts = 
    foldl (\map stmt -> case stmt of
            Assign name expr -> Map.insert name expr map
            Function _ _ _ -> map
         ) Map.empty stmts

extractFunctions :: [Statement] -> [Fun]
extractFunctions stmts = 
    reverse (foldl (\lst stmt -> case stmt of
        Assign _ _ -> lst
        Function name args expr -> (name, args, expr) : lst
     ) [] stmts)

sub :: Map String Expression -> Expression -> Expression
sub vars e = case e of
    Var name -> case Map.lookup name vars of
        Just expr -> sub vars expr
        Nothing -> e
    Const _ -> e
    BinaryOp op e1 e2 -> BinaryOp op (sub vars e1) (sub vars e2)
    Call name exprs -> Call name (map (sub vars) exprs)

verify :: [String] -> Expression -> Expression
verify args expr = case expr of
    Var str -> if elem str args then expr else error "Not all vars bound!"
    Const _ -> expr
    BinaryOp op e1 e2 -> BinaryOp op (verify args e1) (verify args e2)
    Call name exprs -> Call name (map (verify args) exprs)

substitute :: [Statement] -> [Fun]
substitute stmts = 
    let vars = extractVars stmts in
    let funs = extractFunctions stmts in
    let subhlp = (\(name, args, expr) -> (name, args, verify args (sub vars expr))) in
    map subhlp funs
