module TargetSQL where
import Data.List
import AST

compileOp :: BinOp -> String
compileOp op = case op of
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
    Exp -> fail "Special case replace with function"

compileExpr :: Expression -> String
compileExpr expr = case expr of
    Var str -> str
    Const x -> show x
    BinaryOp op e1 e2 -> case op of
        Exp -> compileExpr (Call "POW" [e1, e2])
        _ -> "(" ++ (compileExpr e1) ++ ")" ++ (compileOp op) ++ "(" ++ (compileExpr e2) ++ ")"
    Call name exprs -> name ++ "(" ++ (intercalate "," (map compileExpr exprs)) ++ ")"

compileFun :: Fun -> String
compileFun (name, args, body) = 
    let arglist = intercalate ", " (map (\n -> n ++ " FLOAT64") args) in
    "CREATE TEMPORARY FUNCTION " ++ name ++ "(" ++ arglist ++ ") AS (" ++ (compileExpr body) ++ ");"

compile :: [Fun] -> String
compile funs = 
    intercalate "\n" (map compileFun funs)