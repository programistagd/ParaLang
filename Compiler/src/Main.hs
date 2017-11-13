module Main where

import System.Environment
import System.IO
import Parser
import Dynamic.DynamicCompiler
import Text.Show.Pretty

printInLines :: Show a => [a] -> IO ()
printInLines [] = return ()
printInLines (h:t) = do
  print h
  printInLines t

process :: String -> IO ()
process filename = do
  contents <- readFile filename
  let ast = Parser.parseProgram contents filename
  hPutStrLn stderr (ppShow ast)
  putStrLn $ compile ast

main :: IO ()
main = do
  args <- getArgs
  case args of
    fname:_ -> process fname
    _ -> putStrLn "Please specify filename" 
