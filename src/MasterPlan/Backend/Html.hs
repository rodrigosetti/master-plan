{-|
Module      : MasterPlan.Backend.Html
Description : a backend that renders to a UI HTML
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module MasterPlan.Backend.Html (render) where

import           MasterPlan.Data
import qualified Data.Text          as T

render ∷ ProjectSystem → [ProjProperty] -> T.Text
render = error "not implemented"
