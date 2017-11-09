module Main where

import System.Environment
import Parser

printInLines :: Show a => [a] -> IO ()
printInLines [] = return ()
printInLines (h:t) = do
  print h
  printInLines t

compile :: String -> IO ()
compile filename = do
  contents <- readFile filename
  --printInLines (Parser.parseProgram contents)
  print (Parser.parseProgram contents filename)

main :: IO ()
main = do
  args <- getArgs
  case args of
    fname:_ -> compile fname
    _ -> putStrLn "Please specify filename"
