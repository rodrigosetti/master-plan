{-|
Module      : MasterPlan.Backend.Text
Description : a backend that renders to a UI text
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.Backend.Text (render) where

import           MasterPlan.Data

render ∷ ProjectSystem → [ProjProperty] -> String
render = error "not implemented"
