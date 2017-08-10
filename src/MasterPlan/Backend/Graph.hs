{-|
Module      : MasterPlan.Backend.Graph
Description : a backend that renders to PNG diagram
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.Backend.Graph (render) where

import           MasterPlan.Data

render ∷ FilePath -> ProjectSystem → [ProjProperty] -> IO ()
render = error "not implemented"
