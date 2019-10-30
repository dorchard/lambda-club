module Lam where

import Lam.Parser      (parseProgram)
import Lam.PrettyPrint (pprint)
import Lam.Semantics   (multiStep)
import Lam.Syntax      (isTyped)
import Lam.Types

import System.Directory   (doesPathExist)
import System.Environment (getArgs)
import Control.Monad      (when)

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
          case parseProgram fname input of
            Right (ast, options) -> do
              -- Show AST
              putStrLn $ "\n AST: " <> show ast
              -- Pretty print
              putStrLn $ "\n Pretty: " <> pprint ast
              -- Evaluate
              let (normalForm, count) = multiStep ast
              putStrLn $ "\n Number of steps: " <> show count
              putStrLn $ "\n Normal form: " <> pprint normalForm
              -- Typing
              when (isTyped options)
                  (case synth [] ast of
                     Nothing -> putStrLn "\n Not well-typed!"
                     Just ty -> putStrLn $ "\n Type is: " <> pprint ty)
            Left msg -> error msg