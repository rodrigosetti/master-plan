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
module MasterPlan.Internal.Debug ( debugSys , debugProj) where

import           Control.Monad   (forM_, replicateM_, void)
import qualified Data.Map        as M
import           MasterPlan.Data
import           Text.Printf     (printf)

-- * Debugging

-- |Print a ProjectSystem to standard output
debugSys ∷ ProjectSystem → IO ()
debugSys sys@(ProjectSystem bs) = void $ M.traverseWithKey printBinding bs
  where
    printBinding key b = do putStrLn "-------------------"
                            putStr $ key ++ " = "
                            case b of
                              BindingExpr _ e -> putStr "\n" >> debugProj sys e
                              BindingAtomic _ c t p -> putStrLn $ printf "(c:%.2f,t:%.2f,p:%2.f)" c t p
                              BindingPlaceholder _ -> putStrLn "?"

-- |Print a Project Expression in a Project System to standard output.
-- The expression is printed in a tree like fashion.
debugProj ∷ ProjectSystem → ProjectExpr → IO ()
debugProj sys = print' 0
   where
     ident ∷ Int → IO ()
     ident il = replicateM_ il $ putStr " |"

     print' ∷ Int → ProjectExpr → IO ()
     print' il p@(Reference n)  = ident il >> putStr ("-" ++ n ++ "   ") >> ctp p
     print' il p@(Sum ps) = ident il >> putStr "-+   " >> ctp p >> forM_ ps (print' $ il+1)
     print' il p@(Sequence ps) = ident il >> putStr "->   " >> ctp p >> forM_ ps (print' $ il+1)
     print' il p@(Product ps) = ident il >> putStr "-*   " >> ctp p >> forM_ ps (print' $ il+1)

     ctp p = putStrLn $ printf " c=%.2f  t=%.2f  p=%.2f" (cost sys p) (trust sys p) (progress sys p)
