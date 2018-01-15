module Main where

import System.Environment
import System.IO
import Parser
-- import Dynamic.DynamicCompiler
import Text.Show.Pretty
import AST
import Substitution
import Simplify
import TargetSQL

printInLines :: Show a => [a] -> IO ()
printInLines [] = return ()
printInLines (h:t) = do
  print h
  printInLines t

process :: String -> IO ()
process filename = do
  contents <- readFile filename;
  let ast = Parser.parseProgram contents filename;
  hPutStrLn stderr "Parsed AST:";
  hPutStrLn stderr (ppShow ast);
  let subbed = Substitution.substitute ast;
  hPutStrLn stderr (ppShow subbed);
  let simpl = Simplify.simplify subbed
  hPutStrLn stderr (ppShow simpl);
  hPutStrLn stdout (TargetSQL.compile simpl)

main :: IO ()
main = do
  args <- getArgs
  case args of
    fname:_ -> process fname
    _ -> putStrLn "Please specify filename"
