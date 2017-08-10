{-|
Module      : MasterPlan.Backend.Identity
Description : a backend that renders to a text that can be parsed
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
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
render ∷ ProjectSystem → [ProjProperty] -> String
render (ProjectSystem bs) whitelist =
   snd $ evalRWS (renderName "root" >> renderRest) whitelist bs
 where
   renderRest = gets M.keys >>= mapM_ renderName

type RenderMonad = RWS [ProjProperty] String (M.Map String ProjectBinding)

renderLine ∷ String → RenderMonad ()
renderLine s = tell $ s ++ ";\n"

renderName ∷ ProjectKey → RenderMonad ()
renderName projName =
  do mb <- gets $ M.lookup projName
     case mb of
       Nothing -> pure ()
       Just b  -> do rendered <- renderBinding projName b
                     when rendered $ tell "\n" -- empty line to separate bindings
                     modify $ M.delete projName
                     mapM_ renderName $ dependencies b

dependencies ∷ ProjectBinding → [ProjectKey]
dependencies = nub . everything (++) ([] `mkQ` collectDep)
  where
    collectDep (RefProj n) = [n]
    collectDep _           = []

renderProps ∷ String → ProjectProperties → RenderMonad Bool
renderProps projName p = or <$> sequence [ renderProperty projName PTitle (title p) projName show
                                         , renderProperty projName PDescription (description p) Nothing (show . fromMaybe "")
                                         , renderProperty projName PUrl (url p) Nothing (show . fromMaybe "")
                                         , renderProperty projName POwner (owner p) Nothing (show . fromMaybe "") ]

renderProperty ∷ Eq a ⇒ ProjectKey → ProjProperty → a → a → (a → String) → RenderMonad Bool
renderProperty projName prop val def toStr
  | val == def = pure False
  | otherwise = do whitelisted <- asks (prop `elem`)
                   when whitelisted $
                        renderLine $ show prop ++ "(" ++ projName ++ ") = " ++ toStr val
                   pure whitelisted

renderBinding ∷ ProjectKey → ProjectBinding → RenderMonad Bool
renderBinding projName (UnconsolidatedProj p) = renderProps projName p
renderBinding projName (TaskProj props c t p) =
    or <$> sequence [ renderProps projName props
                    , renderProperty projName PCost c 0 show
                    , renderProperty projName PTrust t 1 percentage
                    , renderProperty projName PProgress p 0 percentage ]
  where
    percentage n = show (n * 100) ++ "%"


renderBinding projName (ExpressionProj pr e) =
    do void $ renderProps projName pr
       renderLine $ projName ++ " = " ++ expressionToStr False e
       pure True
  where
    combinedEToStr parens op ps = let sube = map (expressionToStr True) $ NE.toList ps
                                      s = intercalate (" " ++ op ++ " ") sube
                                   in if parens && length ps > 1 then "(" ++ s ++ ")" else s

    expressionToStr _      (RefProj n)       = n
    expressionToStr parens (ProductProj ps)  = combinedEToStr parens "*" ps
    expressionToStr parens (SequenceProj ps) = combinedEToStr parens "->" ps
    expressionToStr parens (SumProj ps)      = combinedEToStr parens "+" ps
