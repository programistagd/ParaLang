{- This module implements a very crude and simple bytecode compiler
   that outputs a bytecode similar to dynamic languages
   - there are no checks for existence of identifiers or types
   - there are original variable names in the "bytecode" which is a simple text file

   VM assumptions
   For simplicity I assume the VM has infinite amount of registers
   Currently there are no optimizations in usage of registers because I'm not doing this to learn compiler optimization (right now, that'll come but later)

   Bytecode ref:
   load name reg - loads variable to register `reg`
   seti value reg - loads integer value into register
   sets value reg - loads string value into register
   `BinOP` regA regB regR -> regR := regA OP regB
   puts reg - put value from reg onto call-stack
   call reg - call a function from stack (gets its name and then all parameters from the stack) and put result into reg
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

addArg :: (Registers, String, [Integer]) -> Expression -> (Registers, String, [Integer])
addArg (env, code, regs) expr =
    let (nenv, ncode, nreg) = compileExpression env expr
    in (nenv, code @@ ncode, nreg : regs)

compileExpression :: Registers -> Expression -> (Registers, String, Integer)
compileExpression env expr = case expr of
    Var name -> (nenv, "load" %% name %% show reg, reg)
        where (nenv, reg) = regGetOrAdd env name
    IntConst x -> (nenv, "seti" %% show x %% show reg, reg)
        where (nenv, reg) = regGetAnon env
    StrConst s -> (nenv, "sets" %% s %% show reg, reg)
        where (nenv, reg) = regGetAnon env
    BinaryOp op a b -> let (aenv, acode, areg) = compileExpression env a in
                       let (benv, bcode, breg) = compileExpression aenv b in
                       let (nenv, rreg) = regGetAnon benv in
                       (nenv, acode @@ bcode @@
                              show op %% show areg %% show breg %% show rreg, rreg)
    Call name args ->  let (aenv, acode, regs) = foldl addArg (env, "", []) args in
                       let rcode = foldl (@@) acode (map (\reg -> "puts" %% show reg) regs) in
                       let (env1, nreg) = regGetAnon aenv in
                       let (nenv, rreg) = regGetAnon env1 in
                       (nenv, rcode @@
                              "sets" %% name %% show nreg @@
                              "puts" %% show nreg @@
                              "call" %% show rreg, rreg)

loadArgs :: [String] -> (Registers, String)
loadArgs args = (regEmpty, "(???)")

compileStatement :: Statement -> Registers -> String
compileStatement stmt env = "{}"

compileDef :: Definition -> String
compileDef (Function name args body) =
    "fun" %% name @@
    let (env, lcode) = loadArgs args in
    lcode @@
    compileStatement body env @@
    "endfun"
compileDef (GlobalVar name value) =
    expr @@ "glob" %% name %% show reg
    where (_, expr, reg) = compileExpression regEmpty value


compile :: [Definition] -> String
compile = foldr ((@@) . compileDef) ""
