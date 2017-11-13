module Mods.Concurrency where

import AST

modifyCall :: Expression -> Expression
modifyCall (Call "launch" exprs) = case exprs of
    [Call name args] -> Call "launch" (StrConst name : IntConst (fromIntegral $ length args) : args)
    _ -> error ("launch function expects one argument - a function invocation to be delegated, got:\n" ++ unlines (map show exprs))
modifyCall (Call s exprs) = Call s (map modifyExpr exprs)
modifyCall _ = undefined

modifyExpr :: Expression -> Expression
modifyExpr expr = case expr of
    Call {} -> modifyCall expr -- delegate it because more logic happens here
    BinaryOp op a b -> BinaryOp op (modifyExpr a) (modifyExpr b)
    AccessArray a b -> AccessArray (modifyExpr a) (modifyExpr b)
    DotExpr a b -> DotExpr (modifyExpr a) (modifyExpr b)
    _ -> expr -- otherwise identity

modifyStmt :: Statement -> Statement
modifyStmt stmt = case stmt of
  Sequence stmts -> Sequence $ map modifyStmt stmts
  Assign a b -> Assign (modifyExpr a) (modifyExpr b)
  If c a b -> If (modifyExpr c) (modifyStmt a) (modifyStmt b)
  While c a -> While (modifyExpr c) (modifyStmt a)
  Eval e -> Eval $ modifyExpr e
  Return e -> Return $ modifyExpr e
  NoOp -> NoOp

makeMonitor :: String -> [Definition] -> Definition
makeMonitor name defs = Structure name "__TODO__" defs -- TODO

modifyStructure :: Definition -> Definition
modifyStructure (Structure name "monitor" defs) = makeMonitor name defs
modifyStructure (Structure name typename defs) = Structure name typename (modify defs)
modifyStructure _ = undefined

modifyDef :: Definition -> Definition
modifyDef def = case def of
    Function a b stmt -> Function a b (modifyStmt stmt)
    GlobalVar a expr -> GlobalVar a (modifyExpr expr)
    Structure {} -> modifyStructure def

modify :: [Definition] -> [Definition]
modify = map modifyDef
