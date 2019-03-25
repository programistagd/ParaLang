module Interpreter where

import Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import AST


data IValue = VStr String | VInt Integer | VNull deriving Show -- TODO

type Loc = Int


type IFun = [IValue] -> RState IValue
data IEnv = IEnv { vars :: Map String Loc, funcs :: Map String IFun }
type IState = Map Loc IValue

type IValuation = String -> IValue

type RState = StateT IState (ReaderT IEnv IO)

unsafeLookup :: Show k => Ord k => k -> Map k v -> v
unsafeLookup k m = case M.lookup k m of
  Just v -> v
  Nothing -> error (show k ++ " not found")

evalExp :: Expression -> RState IValue
evalExp (Var s) = readVar s
evalExp (IntConst i) = return $ VInt i
evalExp (StrConst s) = return $ VStr s
evalExp (BinaryOp op e1 e2) = evalExp $ Call (show op) [e1, e2]
evalExp (Call fname args) = do
  args' <- mapM evalExp args
  f <- findFunc fname
  f args'
  where
    findFunc :: String -> RState IFun
    findFunc name = do
      ef <- asks funcs
      let (Just f) = M.lookup name ef
      return f
evalExp (AccessArray _ _) = error "Arrays not supported"
evalExp (DotExpr _ _) = error "Objects not supported"

readVar :: String -> RState IValue
readVar s = do
  ve <- asks vars
  let (Just loc) = M.lookup s ve -- TODO error handling
  gets $ findWithDefault VNull loc

evalStatement :: Statement -> () -> IO () -- TODO
evalStatement = undefined

callFunction :: String -> IEnv -> IO IValue
callFunction name env = runReaderT (evalStateT (evalExp fun) initialState) env where
  fun = Call name []
  initialState = M.empty

parseDefinitions :: IEnv -> [Definition] -> IEnv
parseDefinitions initial defs = initial -- TODO

initialEnv :: IEnv
initialEnv = IEnv M.empty $ M.fromList funcs where
  funcs :: [(String, IFun)]
  funcs = [
    ("print", \args -> lift $ lift $ print args >> return VNull)
          ]
