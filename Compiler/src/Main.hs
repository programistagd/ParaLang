module Main where

import System.Environment
import Parser
import Dynamic.DynamicCompiler

printInLines :: Show a => [a] -> IO ()
printInLines [] = return ()
printInLines (h:t) = do
  print h
  printInLines t

process :: String -> IO ()
process filename = do
  contents <- readFile filename
  let ast = Parser.parseProgram contents filename
  putStrLn $ compile ast

main :: IO ()
main = do
  args <- getArgs
  case args of
    fname:_ -> process fname
    _ -> putStrLn "Please specify filename"
