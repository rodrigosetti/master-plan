{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           MasterPlan.Backend.Identity
import           MasterPlan.Parser

main ∷ IO ()
main = do let filename = "master.plan"
          contents <- readFile filename
          case runParser filename contents of
            Left e  -> putStr e
            Right v -> putStr $ render v
