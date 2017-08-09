{-|
Module      : MasterPlan.Backend.Identity
Description : a backend that renders to a text that can be parsed
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@email.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.Backend.Identity (render) where

import           Control.Monad.RWS
import           Data.Generics
import           Data.List          (intercalate, nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe)
import           MasterPlan.Data

-- |Plain text renderer
render ∷ ProjectSystem → String
render (ProjectSystem bs) =
   snd $ evalRWS (renderName "root" >> renderRest) () bs
 where
   renderRest = gets M.keys >>= mapM_ renderName

type RenderMonad = RWS () String (M.Map String ProjectBinding)

renderLine ∷ String → RenderMonad ()
renderLine s = tell $ s ++ ";\n"

renderName ∷ ProjectKey → RenderMonad ()
renderName projName =
  do mb <- gets $ M.lookup projName
     case mb of
       Nothing -> pure ()
       Just b  -> do renderBinding projName b
                     tell "\n" -- empty line to separate bindings
                     modify $ M.delete projName
                     mapM_ renderName $ dependencies b

dependencies ∷ ProjectBinding → [ProjectKey]
dependencies = nub . everything (++) ([] `mkQ` collectDep)
  where
    collectDep (RefProj n) = [n]
    collectDep _           = []

renderProps ∷ String → ProjectProperties → RenderMonad ()
renderProps projName p = do renderProperty projName "name" (title p) projName show
                            renderProperty projName "description" (description p) Nothing (show . fromMaybe "")
                            renderProperty projName "url" (url p) Nothing (show . fromMaybe "")
                            renderProperty projName "owner" (owner p) Nothing (show . fromMaybe "")

renderProperty ∷ Eq a ⇒ ProjectKey → String → a → a → (a → String) → RenderMonad ()
renderProperty projName propName val def toStr
  | val == def = pure ()
  | otherwise = renderLine $ propName ++ "(" ++ projName ++ ") = " ++ toStr val

renderBinding ∷ ProjectKey → ProjectBinding → RenderMonad ()
renderBinding projName (UnconsolidatedProj p) = renderProps projName p
renderBinding projName (TaskProj props c t p) =
    do renderProps projName props
       renderProperty projName "cost" c 0 show
       renderProperty projName "trust" t 1 percentage
       renderProperty projName "progress" p 0 percentage
  where
    percentage n = show (n * 100) ++ "%"


renderBinding projName (ExpressionProj pr e) =
    do renderProps projName pr
       renderLine $ projName ++ " = " ++ expressionToStr False e
  where
    combinedEToStr parens op ps = let sube = map (expressionToStr True) $ NE.toList ps
                                      s = intercalate (" " ++ op ++ " ") sube
                                   in if parens && length ps > 1 then "(" ++ s ++ ")" else s

    expressionToStr _      (RefProj n)       = n
    expressionToStr parens (ProductProj ps)  = combinedEToStr parens "*" ps
    expressionToStr parens (SequenceProj ps) = combinedEToStr parens "->" ps
    expressionToStr parens (SumProj ps)      = combinedEToStr parens "+" ps
