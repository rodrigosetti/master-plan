{-|
Module      : MasterPlan.Internal.Debug
Description : Debugging functions
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.Internal.Debug (printProject) where

import           Control.Monad   (forM_, replicateM_)
import           Data.Maybe      (fromMaybe)
import           MasterPlan.Data
import           Text.Printf     (printf)

-- * Debugging

-- |Print a Project Expression in a Project System to standard output.
-- The expression is printed in a tree like fashion.
printProject ∷ ProjectExpr → IO ()
printProject = print' 0
   where
     ident ∷ Int → IO ()
     ident il = replicateM_ il $ putStr " |"

     print' ∷ Int → ProjectExpr → IO ()
     print' il p@(Atomic r _ _ _)  = ident il >> putStr ("-" ++ fromMaybe "?" (title r) ++ "   ") >> ctp p
     print' il p@(Sum _ ps) = ident il >> putStr "-+   " >> ctp p >> forM_ ps (print' $ il+1)
     print' il p@(Sequence _ ps) = ident il >> putStr "->   " >> ctp p >> forM_ ps (print' $ il+1)
     print' il p@(Product _ ps) = ident il >> putStr "-*   " >> ctp p >> forM_ ps (print' $ il+1)
     print' _  (Annotated _ ) = undefined

     ctp p = putStrLn $ printf " c=%.2f  t=%.2f  p=%.2f"
                               (getCost $ cost p)
                               (getTrust $ trust p)
                               (getProgress $ progress p)
