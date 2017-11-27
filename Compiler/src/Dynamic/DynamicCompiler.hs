{- This module implements a very crude and simple bytecode compiler
   that outputs a bytecode similar to dynamic languages
   - there are no checks for existence of identifiers or types
   - there are original variable names in the "bytecode" which is a simple text file

   VM specification can be found in /docs/Dynamic.md
-}
module Dynamic.DynamicCompiler where

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
    let (nenv, ncode, nreg) = compileExpressionValue env expr
    in (nenv, code @@ ncode, nreg : regs)

compileExpressionValue :: Registers -> Expression -> (Registers, String, Int)
compileExpressionValue env expr = case expr of
    Var name -> (nenv, "load" %% name %% show reg, reg)
        where (nenv, reg) = regGetOrAdd env name
    IntConst x -> (nenv, "seti" %% show x %% show reg, reg)
        where (nenv, reg) = regGetAnon env
    StrConst s -> (nenv, "sets" %% show reg %% show s, reg)
        where (nenv, reg) = regGetAnon env
    BinaryOp op a b -> let (aenv, acode, areg) = compileExpressionValue env a in
                       let (benv, bcode, breg) = compileExpressionValue aenv b in
                       let (nenv, rreg) = regGetAnon benv in
                       (nenv, acode @@ bcode @@
                              show op %% show areg %% show breg %% show rreg, rreg)
    Call name args ->  let (aenv, acode, regs) = foldl exprPushArg (env, "", []) args in
                       let rcode = foldl (@@) acode (map (\reg -> "push" %% show reg) regs) in
                       let (nenv, rreg) = regGetAnon aenv in
                       (nenv,
                       rcode @@
                       "puts" %% name @@
                       "call" %% show rreg,
                       rreg)
    AccessArray arrexp indexp -> let (aenv, acode, areg) = compileExpressionValue env arrexp in
                                 let (ienv, icode, ireg) = compileExpressionValue aenv indexp in
                                 let (nenv, rreg) = regGetAnon ienv in
                               (nenv,
                               acode @@
                               icode @@
                               "loada" %% show areg %% show ireg %% show rreg,
                               rreg)
    DotExpr left (Var name) -> let (lenv, lcode, lreg) = compileExpressionValue env left in
                               let (nenv, rreg) = regGetAnon lenv in
                               (nenv,
                               lcode @@
                               "open" %% show lreg @@
                               "load" %% name %% show rreg @@
                               "close",
                               rreg)
    DotExpr left (Call name args) -> let (lenv, lcode, lreg) = compileExpressionValue env left in
                                     let (aenv, acode, regs) = foldl exprPushArg (lenv, "", []) args in
                                     let rcode = foldl (@@) acode (map (\reg -> "push" %% show reg) regs) in
                                     let (nenv, rreg) = regGetAnon aenv in
                                     (nenv,
                                     lcode @@
                                     rcode @@
                                     "puts" %% name @@
                                     "open" %% show lreg @@
                                     "call" %% show rreg @@
                                     "close",
                                     rreg)
    DotExpr _ _ -> error ("Not allowed construct:" @@ show expr)

loadArg :: String -> String
loadArg name = "pop 0" @@ "save 0" %% name

loadArgs :: [String] -> String
loadArgs args = foldl (@@) "" (map loadArg args)

saveToRef :: Registers -> Expression -> Int -> String
saveToRef env refexp reg = case refexp of
    Var name -> "save" %% show reg %% name
    AccessArray (Var arrayName) indexexp -> let (ienv, icode, ireg) = compileExpressionValue env indexexp in
                                            let (_, areg) = regGetAnon ienv in
                                            icode @@
                                            "load" %% arrayName %% show areg @@
                                            "savea" %% show areg %% show ireg %% show reg
    DotExpr left right -> let (lenv, lcode, lreg) = compileExpressionValue env left in
                         lcode @@
                         "open" %% show lreg @@
                         saveToRef lenv right reg @@
                         "close"
    _ -> error ("Invalid operation, cannot assign to:" @@ show refexp)

compileStatement :: Statement -> String
compileStatement stmt = case stmt of
   Sequence statements -> foldl (@@) "" (map compileStatement statements)
   Assign ref vexpr -> let (env, ecode, rreg) = compileExpressionValue regEmpty vexpr in
                      ecode @@
                      saveToRef env ref rreg
   If condition success failure -> let (env, ccode, creg) = compileExpressionValue regEmpty condition in
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
   While condition body -> let (env, ccode, creg) = compileExpressionValue regEmpty condition in
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
   Eval expr -> let (_, code, _) = compileExpressionValue regEmpty expr in code
   Return expr -> let (_, ecode, ereg) = compileExpressionValue regEmpty expr in
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
    where (_, expr, reg) = compileExpressionValue regEmpty value
compileDef (Structure name "" defs) =
    "struct" %% name @@
    foldl (@@) "" (map compileDef defs) @@
    "endstruct"
compileDef (Structure _ typename _) = error $ "Sugared structures (" ++ typename ++ ") are not supported by base. Maybe you forgot an extension?"


compile :: [Definition] -> String
compile = foldr ((@@) . compileDef) ""
