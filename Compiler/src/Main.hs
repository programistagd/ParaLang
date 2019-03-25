module Main where

import System.Environment
import System.IO
import Parser
import Dynamic.DynamicCompiler
import Text.Show.Pretty
import qualified Mods.Concurrency
import AST
import Interpreter
import Data.Map as M

usedModifications :: [[Definition] -> [Definition]]
usedModifications = [Mods.Concurrency.modify]

printInLines :: Show a => [a] -> IO ()
printInLines [] = return ()
printInLines (h:t) = do
  print h
  printInLines t

process :: String -> IO ()
process filename = do
  contents <- readFile filename;
  let ast = Parser.parseProgram contents filename
  hPutStrLn stderr "Parsed AST:";
  hPutStrLn stderr (ppShow ast);
  let modast = Prelude.foldl (\ast modder -> modder ast) ast usedModifications in
    let modmsg = if ast == modast then "No modifications detected." else "Modded AST:\n" ++ ppShow modast in
    do
      hPutStrLn stderr modmsg;
      -- putStrLn $ compile modast
      let env = parseDefinitions initialEnv modast
      callFunction "print" env >>= print

main :: IO ()
main = do
  args <- getArgs
  case args of
    fname:_ -> process fname
    _ -> putStrLn "Please specify filename"
