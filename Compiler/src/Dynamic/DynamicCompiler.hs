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
(@@) a "" = a
(@@) a b = a ++ "\n" ++ b

(%%) :: String -> String -> String
(%%) "" b = b
(%%) a b = a ++ " " ++ b

opCount :: String -> Int -- computes amount of operations in a snippet of machine code
opCount code = length $ filter (=='\n') code

--useReg :: Registers -> String -> String
binaryOpName :: BinOp -> String
binaryOpName = show -- TODO?

exprPushArg :: (Registers, String, [Int]) -> Expression -> (Registers, String, [Int])
exprPushArg (env, code, regs) expr =
    let (nenv, ncode, nreg) = compileExpression env expr
    in (nenv, code @@ ncode, nreg : regs)

compileExpression :: Registers -> Expression -> (Registers, String, Int)
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

compileStatement :: Statement -> String
compileStatement stmt = case stmt of
   Sequence statements -> foldl (@@) "" (map compileStatement statements)
   Assign name expr -> let (_, code, rreg) = compileExpression regEmpty expr in
                       code @@ "save" %% show rreg %% name
   If condition success failure -> let (env, ccode, creg) = compileExpression regEmpty condition in
                              let scode = compileStatement success in
                              let fcode = compileStatement failure in
                              let slen = opCount scode in
                              let flen = opCount fcode in
                              let (_, jreg) = regGetAnon env in
                              ccode @@ -- compute condition value
                              "sets" %% show (slen + 3) %% show jreg @@
                              "if" %% show creg %% show jreg @@ -- if condition is 0 (false) jumps to else
                              scode @@ -- executed if no jump occured
                              "sets" %% show (flen + 1) %% show jreg @@
                              "jmp" %% show jreg @@ -- after success branch jump over failure branch
                              fcode
   While condition body -> let (env, ccode, creg) = compileExpression regEmpty condition in
                      let bcode = compileStatement body in
                      let clen = opCount ccode in
                      let blen = opCount bcode in
                      let (_, jreg) = regGetAnon env in
                      ccode @@
                      "sets" %% show (blen + 1) %% show jreg @@
                      "if" %% show creg %% show jreg @@
                      bcode @@
                      "sets" %% show (- (blen + 1 + 2 + clen)) %% show jreg @@
                      "jmp" %% show jreg
   Eval expr -> let (_, code, _) = compileExpression regEmpty expr in code
   Return expr -> let (_, ecode, ereg) = compileExpression regEmpty expr in
                  ecode @@ "retrn" %% show ereg
   NoOp -> ""

compileDef :: Definition -> String
compileDef (Function name args body) =
    "func" %% name @@
    loadArgs args @@
    compileStatement body @@
    "endfunc"
compileDef (GlobalVar name value) =
    expr @@ "glob" %% name %% show reg
    where (_, expr, reg) = compileExpression regEmpty value


compile :: [Definition] -> String
compile = foldr ((@@) . compileDef) ""
