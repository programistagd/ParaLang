module TargetLaTeX where
import Data.List
import AST
import Text.Printf
import qualified Data.Text as Text

showNum :: Double -> String
showNum x = if abs (x - fromIntegral (floor x)) <= 0.1 then show (floor x) else printf "%.2f" x

startsWith :: String -> String -> Bool
startsWith ptrn txt = case (ptrn, txt) of
    (a : [], b : _)  | a == b -> True
    (a : pe, b : te) | a == b -> startsWith pe te
    _ -> False

showVar :: String -> String
showVar s = 
    let greek = ["alpha", "beta", "gamma", "delta", "eta"] in
    let tryreplace s g = if startsWith g s then (if g == s then "\\" ++ g else (Text.unpack (Text.replace (Text.pack g) (Text.pack ("\\" ++ g ++ "_{")) (Text.pack s))) ++ "}") else s in
    foldl tryreplace s greek

data Priority = High | Low

brace :: String -> Priority -> String -- add braces if expression is of low priority
brace expr High = expr
brace expr Low = "(" ++ expr ++ ")"

compileExpr :: Expression -> (String, Priority)
compileExpr expr = case expr of
    Var str -> (showVar str, High)
    Const x -> (showNum x, High)
    BinaryOp op e1 e2 -> 
        let (l, pl) = compileExpr e1 in
        let (r, pr) = compileExpr e2 in
        case op of
            Add -> (l ++ "+" ++ r, Low)
            Subtract -> (l ++ "-" ++ (brace r pr), Low)
            Multiply -> ((brace l pl) ++ "\\cdot " ++ (brace r pr), High)
            Divide -> ("\\frac{" ++ l ++ "}{" ++ r ++ "}", High)
            Exp -> ((brace l pl) ++ "^{" ++ r ++ "}", High)
    Call name exprs -> (name ++ "\\left(" ++ (intercalate "," (map (fst . compileExpr) exprs)) ++ "\\right)" , High)

compileStmt :: Statement -> String
compileStmt stmt = case stmt of
    Assign name expr -> (showVar name) ++ " = " ++ fst (compileExpr expr)
    Function name args expr -> name ++ "(" ++ (intercalate ", " args) ++ ") = " ++ fst (compileExpr expr)

compileLaTeX :: [Statement] -> String
compileLaTeX stmts = 
    "\\[\n" ++ (intercalate " \\\\\n" (map compileStmt stmts)) ++ "\n\\]"