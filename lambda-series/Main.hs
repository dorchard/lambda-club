module Lam where

import Lam.Parser      (parseProgram)
import Lam.PrettyPrint (pprint)
import Lam.Semantics   (multiStep)

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
            Right ast -> do
              -- Show AST
              putStrLn $ "\n AST: " <> show ast
              -- Pretty print
              putStrLn $ "\n Pretty: " <> pprint ast
              -- Evaluate
              putStrLn $ "\n Normal form: " <> pprint (multiStep ast)
            Left msg -> error msg