{-|
Module      : MasterPlan.Backend.Identity
Description : a backend that renders to a text that can be parsed
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module MasterPlan.Backend.Identity (render) where

import           Control.Monad      (when)
import           Control.Monad.RWS  (RWS, evalRWS, gets, modify, tell)
import           Data.Generics
import           Data.List          (nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           Data.Maybe         (isJust)
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           MasterPlan.Data

-- |Plain text renderer
render ∷ ProjectSystem → [ProjAttribute] -> T.Text
render (ProjectSystem bs) whitelist =
   snd $ evalRWS (renderName "root" >> renderRest) whitelist bs
 where
   renderRest = gets M.keys >>= mapM_ renderName

type RenderMonad = RWS [ProjAttribute] T.Text (M.Map ProjectKey Binding)

renderName ∷ ProjectKey → RenderMonad ()
renderName projName =
  do mb <- gets $ M.lookup projName
     case mb of
       Nothing -> pure ()
       Just b  -> do tell $ T.pack $ getProjectKey projName
                     when (hasAttribute b) $ do
                       tell " {\n"
                       renderAttr b
                       tell "}"
                     case b of
                       BindingExpr _ e -> tell $ " " <> expressionToStr False e <> ";\n"
                       _ -> tell ";\n"
                     modify $ M.delete projName
                     mapM_ renderName $ dependencies b
 where
   hasAttribute (BindingExpr props _) = hasProperty props
   hasAttribute (BindingAtomic props c t p) =  hasProperty props
                                            || c /= defaultCost
                                            || t /= defaultTrust
                                            || p  /= defaultProgress
   hasProperty props =  title props /= getProjectKey projName
                     || isJust (description props)
                     || isJust (owner props)
                     || isJust (url props)

   percentage n = T.pack $ show (n * 100) <> "%"

   renderAttr (BindingExpr props _) = renderProps props
   renderAttr (BindingAtomic props c t p) =
     do renderProps props
        when (c /= defaultCost) $ tell $ "cost " <> T.pack (show $ getCost c) <> "\n"
        when (t /= defaultTrust) $ tell $ "trust " <> percentage (getTrust t) <> "\n"
        when (p /= defaultProgress) $ tell $ "progress " <> percentage (getProgress p) <> "\n"

   renderProps :: ProjectProperties -> RenderMonad ()
   renderProps p = do let maybeRender :: T.Text -> Maybe String -> RenderMonad ()
                          maybeRender n = maybe (pure ()) (\x -> tell $ n <> " " <> T.pack (show x) <> "\n")
                      when (title p /= getProjectKey projName) $
                        tell $ "title " <> T.pack (show $ title p) <> "\n"
                      maybeRender "description" (description p)
                      maybeRender "url" (url p)
                      maybeRender "owner" (owner p)

   combinedEToStr parens op ps = let sube = map (expressionToStr True) $ NE.toList ps
                                     s = T.intercalate (" " <> op <> " ") sube
                                  in if parens && length ps > 1 then "(" <> s <> ")" else s

   expressionToStr :: Bool -> ProjectExpr -> T.Text
   expressionToStr _      (Reference (ProjectKey n)) = T.pack n
   expressionToStr parens (Product ps)               = combinedEToStr parens "*" ps
   expressionToStr parens (Sequence ps)              = combinedEToStr parens "->" ps
   expressionToStr parens (Sum ps)                   = combinedEToStr parens "+" ps

dependencies ∷ Binding → [ProjectKey]
dependencies = nub . everything (++) ([] `mkQ` collectDep)
  where
    collectDep (Reference n) = [n]
    collectDep _             = []
