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

compileExpr :: Expression -> String
compileExpr expr = case expr of
    Var str -> showVar str
    Const x -> showNum x
    BinaryOp op e1 e2 -> 
        let l = compileExpr e1 in
        let r = compileExpr e2 in
        case op of
            Add -> l ++ "+" ++ r
            Subtract -> l ++ "-" ++ r
            Multiply -> l ++ "\\cdot " ++ r
            Divide -> "\\frac{" ++ l ++ "}{" ++ r ++ "}"
            Exp -> l ++ "^{" ++ r ++ "}"
    Call name exprs -> name ++ "\\left(" ++ (intercalate "," (map compileExpr exprs)) ++ "\\right)"

compileStmt :: Statement -> String
compileStmt stmt = case stmt of
    Assign name expr -> (showVar name) ++ " = " ++ (compileExpr expr)
    Function name args expr -> name ++ "(" ++ (intercalate ", " args) ++ ") = " ++ (compileExpr expr)

compileLaTeX :: [Statement] -> String
compileLaTeX stmts = 
    "\\[\n" ++ (intercalate " \\\\\n" (map compileStmt stmts)) ++ "\n\\]"