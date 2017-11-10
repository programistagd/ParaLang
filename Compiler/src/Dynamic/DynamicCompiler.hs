{- This module implements a very crude and simple bytecode compiler
   that outputs a bytecode similar to dynamic languages
   - there are no checks for existence of identifiers or types
   - there are original variable names in the "bytecode" which is a simple text file

   VM specification can be found in /docs/Dynamic.md
-}
module Dynamic.DynamicCompiler where

import Parser
import AST
import Dynamic.Environment

(@@) :: String -> String -> String
(@@) "" b = b
(@@) a b = a ++ "\n" ++ b

(%%) :: String -> String -> String
(%%) "" b = b
(%%) a b = a ++ " " ++ b

--useReg :: Registers -> String -> String
binaryOpName :: BinOp -> String
binaryOpName = show -- TODO?

exprPushArg :: (Registers, String, [Integer]) -> Expression -> (Registers, String, [Integer])
exprPushArg (env, code, regs) expr =
    let (nenv, ncode, nreg) = compileExpression env expr
    in (nenv, code @@ ncode, nreg : regs)

compileExpression :: Registers -> Expression -> (Registers, String, Integer)
compileExpression env expr = case expr of
    Var name -> (nenv, "load" %% name %% show reg, reg)
        where (nenv, reg) = regGetOrAdd env name
    IntConst x -> (nenv, "seti" %% show x %% show reg, reg)
        where (nenv, reg) = regGetAnon env
    StrConst s -> (nenv, "sets" %% show s %% show reg, reg)
        where (nenv, reg) = regGetAnon env
    BinaryOp op a b -> let (aenv, acode, areg) = compileExpression env a in
                       let (benv, bcode, breg) = compileExpression aenv b in
                       let (nenv, rreg) = regGetAnon benv in
                       (nenv, acode @@ bcode @@
                              show op %% show areg %% show breg %% show rreg, rreg)
    Call name args ->  let (aenv, acode, regs) = foldl exprPushArg (env, "", []) args in
                       let rcode = foldl (@@) acode (map (\reg -> "push" %% show reg) regs) in
                       let (nenv, rreg) = regGetAnon aenv in
                       (nenv, rcode @@
                              "puts" %% name @@
                              "call" %% show rreg, rreg)
    DotExpr _ _ -> error (show expr ++ " currently not supported")

loadArg :: String -> String
loadArg name = "pop 0" @@ "save 0" %% name

loadArgs :: [String] -> String
loadArgs args = foldl (@@) "" (map loadArg args)

compileStatement :: Statement -> Registers -> String
compileStatement stmt env = "{}"

compileDef :: Definition -> String
compileDef (Function name args body) =
    "fun" %% name @@
    loadArgs args @@
    compileStatement body regEmpty @@
    "endfun"
compileDef (GlobalVar name value) =
    expr @@ "glob" %% name %% show reg
    where (_, expr, reg) = compileExpression regEmpty value


compile :: [Definition] -> String
compile = foldr ((@@) . compileDef) ""
