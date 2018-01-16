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
import TargetLaTeX

printInLines :: Show a => [a] -> IO ()
printInLines [] = return ()
printInLines (h:t) = do
  print h
  printInLines t

process :: String -> IO ()
process filename = do
  contents <- readFile filename;
  let ast = Parser.parseProgram contents filename;
  --hPutStrLn stderr "Parsed AST:";
  --hPutStrLn stderr (ppShow ast);
  let subbed = Substitution.substitute ast;
  --hPutStrLn stderr (ppShow subbed);
  let simpl = Simplify.simplify subbed
  --hPutStrLn stderr (ppShow simpl);
  hPutStrLn stderr (TargetLaTeX.compileLaTeX ast);
  hPutStrLn stdout (TargetSQL.compileSQL simpl)

main :: IO ()
main = do
  args <- getArgs
  case args of
    fname:_ -> process fname
    _ -> putStrLn "Please specify filename"
