module Lam where

import Lam.Parser

import System.Directory (doesPathExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  -- Get command line args
  case args of
    [] -> putStrLn "Please supply a filename as a command line argument"
    -- If we have at least one
    (fname:_) -> do
      -- Check if this is a file
      exists <- doesPathExist fname
      if not exists
        then putStrLn $ "Fil `" <> fname <> "` cannot be found."
        else do
          -- Read the file, parse, and do something...
          input <- readFile fname
          putStrLn input
          case parseProgram fname input of
            Right ast -> putStrLn $ "AST: " <> show ast
            Left msg -> error msg