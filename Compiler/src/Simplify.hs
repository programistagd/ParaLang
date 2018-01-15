module Simplify where
import AST

compute :: BinOp -> Double -> Double -> Double
compute op a b = case op of
    Add -> a + b
    Subtract -> a - b
    Multiply -> a * b
    Divide -> a / b

simpl :: Expression -> Expression
simpl e = case e of
    Var str -> e
    Const x -> e
    BinaryOp op e1 e2 -> 
        let s1 = simpl e1 in
        let s2 = simpl e2 in
        case (s1, s2) of 
            (Const x1, Const x2) -> Const (compute op x1 x2)
            _ -> BinaryOp op s1 s2
    Call name exprs -> Call name (map simpl exprs) -- TODO you could also evaluate known functions, but that's beyond scope now

simplify :: [Fun] -> [Fun]
simplify funs = 
    map (\(name, args, body) -> (name, args, simpl body)) funs