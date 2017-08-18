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
import           Control.Monad.RWS  hiding (Product, Sum)
import           Data.List          (intersperse)
import qualified Data.List.NonEmpty as NE
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           MasterPlan.Data

type RenderMonad a = RWS [ProjAttribute] T.Text [(ProjectKey, Project a)]

-- |Plain text renderer
render ∷ Project a → [ProjAttribute] -> T.Text
render proj whitelist =
   snd $ evalRWS (renderDefinition "root" proj) whitelist []
 where
   renderDefinition key p =
     do tell $ T.pack key
        when (hasAttribute p) $ do
           tell " {\n"
           renderAttr p
           tell "}"
        case p of
          Atomic {}    -> pure ()
          Annotated {} -> pure ()
          p'           -> tell " " >> expression False p'
        tell "\n;"
        modify $ filter ((/= key) . fst)
        remainingBindings <- get
        case remainingBindings of
          []        -> pure ()
          (k, p'):_ -> renderDefinition k p'

   expression :: Bool -> Project a -> RenderMonad a ()
   expression parens p@(Product _ ps)  = maybeBinding p $ combinedE parens "*" ps
   expression parens p@(Sequence _ ps) = maybeBinding p $ combinedE parens "->" ps
   expression parens p@(Sum _ ps)      = maybeBinding p $ combinedE parens "+" ps
   expression _      p@Atomic {}       = maybeBinding p $ tell $ T.pack $ mkKey p
   expression _ _                      = pure ()

   maybeBinding :: Project a -> RenderMonad a () -> RenderMonad a ()
   maybeBinding p action
    | hasAttribute p = let key = mkKey p
                        in modify ((key, p):) >> tell (T.pack key)
    | otherwise = action

   mkKey :: Project a -> String
   mkKey (Annotated _)        = "?"
   mkKey (Product props _)    = maybe "?" toId $ title props
   mkKey (Sequence props _)   = maybe "?" toId $ title props
   mkKey (Sum props _)        = maybe "?" toId $ title props
   mkKey (Atomic props _ _ _) =  maybe "?" toId $ title props

   toId :: String -> String
   toId = mconcat . words

   combinedE :: Bool -> T.Text -> NE.NonEmpty (Project a) -> RenderMonad a ()
   combinedE parens op ps = let sube = expression True <$> NE.toList ps
                                s = sequence_ $ intersperse (tell $ " " <> op <> " ") sube
                            in  if parens && length ps > 1
                                    then tell "(" >> s >> tell ")"
                                    else s

   hasAttribute (Annotated _) = False
   hasAttribute (Product props _) = hasProperty props
   hasAttribute (Sequence props _) = hasProperty props
   hasAttribute (Sum props _) = hasProperty props
   hasAttribute (Atomic props c t p) =  hasProperty props
                                            || c /= defaultCost
                                            || t /= defaultTrust
                                            || p  /= defaultProgress

   hasProperty props =  isNonEmpty (title props)
                     || isNonEmpty (description props)
                     || isNonEmpty (owner props)
                     || isNonEmpty (url props)

   isNonEmpty Nothing   = False
   isNonEmpty (Just "") = False
   isNonEmpty (Just _)  = True

   percentage n = T.pack $ show (n * 100) <> "%"

   renderAttr (Annotated _) = pure ()
   renderAttr (Product props _) = renderProps props
   renderAttr (Sequence props _) = renderProps props
   renderAttr (Sum props _) = renderProps props
   renderAttr (Atomic props c t p) =
     do renderProps props
        when (c /= defaultCost) $ tell $ "cost " <> T.pack (show $ getCost c) <> "\n"
        when (t /= defaultTrust) $ tell $ "trust " <> percentage (getTrust t) <> "\n"
        when (p /= defaultProgress) $ tell $ "progress " <> percentage (getProgress p) <> "\n"

   renderProps :: ProjectProperties -> RenderMonad a ()
   renderProps p = do let maybeRender :: T.Text -> Maybe String -> RenderMonad a ()
                          maybeRender _ Nothing   = pure ()
                          maybeRender _ (Just "") = pure ()
                          maybeRender n (Just x)  = tell $ n <> " " <> T.pack (show x) <> "\n"
                      maybeRender "title" (title p)
                      maybeRender "description" (description p)
                      maybeRender "url" (url p)
                      maybeRender "owner" (owner p)
