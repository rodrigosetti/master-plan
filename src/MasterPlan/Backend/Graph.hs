{-|
Module      : MasterPlan.Backend.Graph
Description : a backend that renders to PNG diagram
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}
module MasterPlan.Backend.Graph ( render
                                , renderText
                                , RenderOptions(..)) where

import           Control.Monad.State
import           Data.List                   (intersperse, isSuffixOf)
import           Data.Maybe
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude            hiding (Product, Sum, render)
import           Diagrams.TwoD.Text
import           MasterPlan.Data
import           Text.Printf                 (printf)

-- text :: (TypeableFloat n, Renderable (Text n) b) => String -> QDiagram b V2 n Any
-- text = texterific

leftText :: (TypeableFloat n, Renderable (Text n) b) => String -> QDiagram b V2 n Any
leftText = alignedText 0 0.5

rightText :: (TypeableFloat n, Renderable (Text n) b) => String -> QDiagram b V2 n Any
rightText = alignedText 1 0.5

-- |Render multiline text
multilineText' :: (TypeableFloat n, Renderable (Text n) b)
               => FontSlant
               -> FontWeight
               -> n -- ^line spacing
               -> [String] -- ^the lines of text to render
               -> QDiagram b V2 n Any
multilineText' fs fw lineSpace = vsep lineSpace . map (texterific' fs fw)

-- |Render multiline text
multilineText :: (TypeableFloat n, Renderable (Text n) b)
              => n -- ^line spacing
              -> [String] -- ^the lines of text to render
              -> QDiagram b V2 n Any
multilineText = multilineText' FontSlantNormal FontWeightNormal

-- |Render text with possible overflow by breaking lines and truncating with ...
textOverflow' :: (TypeableFloat n, Renderable (Text n) b)
              => FontSlant
              -> FontWeight
              -> Int -- ^maximum number of lines to break
              -> Int -- ^maximum number of chars per line
              -> n -- ^line spacing
              -> String -- ^the text
              -> QDiagram b V2 n Any
textOverflow' fs fw maxLines maxLineSize lineSpace txt =
    vsep lineSpace $ map (texterific' fs fw) ss
  where
    ss = reverse $ foldl processWord [] $ words txt
    processWord (l:ls) w
      | length (l ++ w) > maxLineSize = if length ls >= maxLines
                                         then (if "..." `isSuffixOf` l then l:ls else (l ++ " ..."):ls)
                                         else w:l:ls
      | otherwise = (l ++ " " ++ w):ls
    processWord [] w = [w]

-- |Render text with possible overflow by breaking lines and truncating with ...
textOverflow :: (TypeableFloat n, Renderable (Text n) b)
              => Int -- ^maximum number of lines to break
              -> Int -- ^maximum number of chars per line
              -> n -- ^line spacing
              -> String -- ^the text
              -> QDiagram b V2 n Any
textOverflow = textOverflow' FontSlantNormal FontWeightNormal

-- |Options for rendering
data RenderOptions = RenderOptions { colorByProgress  :: Bool -- ^Whether to color boxes depending on progress
                                   , renderWidth      :: Integer -- ^The width of the output image
                                   , renderHeight     :: Integer -- ^The height of the output image
                                   , whitelistedAttrs :: [ProjAttribute] -- ^Attributes that should be rendered
                                   } deriving (Eq, Show)

-- | The main rendering function
render ∷ FilePath -> RenderOptions-> ProjectExpr → IO ()
render fp (RenderOptions colorByP w h attrs) proj =
  let dia = evalState (renderProject colorByP attrs proj) []
  in renderRasterific fp (dims2D (fromInteger w) (fromInteger h)) $ bgFrame 1 white $ centerXY dia

-- |Render a multi-line text to file
renderText ∷ FilePath -> RenderOptions-> [String] → IO ()
renderText fp RenderOptions { renderWidth=w, renderHeight=h } ss =
  let dia = multilineText (0.1 :: Float) ss
  in renderRasterific fp (dims2D (fromInteger w) (fromInteger h)) $ bgFrame 1 white $ centerXY dia

-- |Monad that keep state of all projects rendered so far
type AvoidRedundancy = State [ProjectExpr]

renderProject :: Bool -> [ProjAttribute] -> ProjectExpr -> AvoidRedundancy (QDiagram B V2 Double Any)
renderProject _        _     (Annotated _)  = undefined
renderProject colorByP attrs p@Atomic {}    = pure $ alignL $ renderNode colorByP attrs p
renderProject colorByP attrs proj =
    do alreadyRendered <- gets (proj `elem`)
       case title =<< properties proj of
         Just n | alreadyRendered -> pure $ renderReference n
         _ -> do modify (proj:)
                 subtrees <- mapM renderSubTree $ subprojects proj
                 let sizesY = map (diameter unitY) subtrees
                 let headBar = case sizesY of
                                []  -> mempty
                                s:_ -> strutY (s/2)
                 pure $ (strutY (sum sizesY) <> alignL (centerY $ renderNode colorByP attrs proj))
                        |||  (translateX 2 typeSymbol # withEnvelope (mempty :: D V2 Double) <> hrule 4 # lwO 2)
                        |||  centerY (headBar === treeBar sizesY)
                        |||  centerY (vcat subtrees)
  where
    renderSubTree subtree = (hrule 4 # lwO 2 |||) <$> renderProject colorByP attrs subtree
    renderReference refName = text refName <> roundedRect 30 2 0.5 # lwO 2 # fc white # dashingN [0.005, 0.005] 0

    treeBar :: [Double] -> QDiagram B V2 Double Any
    treeBar (s1:s2:ss) = vrule (s1/2) # lwO 2 === vrule (s2/2) # lwO 2 === treeBar (s2:ss)
    treeBar [s1] = strutY (s1/2)
    treeBar _ = mempty

    typeSymbol =
      let txt = case proj of
                    Sum {}      -> text "+"
                    Product {}  -> text "x"
                    Sequence {} -> text "->"
                    _           -> mempty
      in txt # fontSizeL 2 # bold <> extrudeTop 2 (extrudeBottom 2 (roundedRect 3 2 1 # fc white # lwO 1))

renderNode :: Bool -> [ProjAttribute] -> ProjectExpr -> QDiagram B V2 Double Any
renderNode colorByP attrs proj =
   centerY $ extrudeTop 2 $ extrudeBottom 2 nodeDia
  where
    c = cost proj
    t = trust proj
    p = progress proj
    prop = properties proj
    nodeDia =
      let sections = if isJust titleHeader
                      then catMaybes [ headerSection
                                     , descriptionSection
                                     , urlSection
                                     , bottomSection]
                      else maybeToList simplifiedNode
          sectionsWithSep = vcat (intersperse (hrule nodeW # dashingN [0.005, 0.005] 0 # lwO 1) sections)
      in centerY (sectionsWithSep <> boundingRect sectionsWithSep # fc projColor # lwO 2)

    nodeW = 30

    simplifiedNode = case [progressHeader, trustHeader' text, costHeader] of
                          [Nothing, Nothing, Nothing] -> Nothing
                          l -> Just $ strutY 2 <> strutX nodeW <> mconcat (catMaybes l)

    givenProp :: ProjAttribute -> Maybe a -> Maybe a
    givenProp pro x = if pro `elem` attrs then x else Nothing

    headerSection = case [progressHeader, titleHeader, costHeader] of
                        [Nothing, Nothing, Nothing] -> Nothing
                        l -> Just $ strutY 2 <> strutX nodeW <> mconcat (catMaybes l)
    progressHeader = givenProp PProgress $ Just $ displayProgress p # translateX (-nodeW/2 + 1)

    titleHeader :: Maybe (QDiagram B V2 Double Any)
    titleHeader = givenProp PTitle $ prop
                                   >>= title
                                   >>= (pure . centerXY . textOverflow' FontSlantNormal FontWeightBold 1 30 0.1)
    costHeader = givenProp PCost $ Just $ displayCost c # translateX (nodeW/2 - 1)

    descriptionSection, urlSection, bottomSection :: Maybe (QDiagram B V2 Double Any)
    descriptionSection = givenProp PDescription $ prop
                                                >>= description
                                                >>= (pure . centerX . frame 0.3 . textOverflow 3 40 0.1)
    urlSection = givenProp PUrl $ prop
                                >>= url
                                >>= (pure . centerX . frame 0.3 . textOverflow 1 20 0)

    bottomSection = case [trustHeader, ownerHeader] of
                      [Nothing, Nothing] -> Nothing
                      l -> Just $ strutY 2 <> strutX nodeW <> mconcat (catMaybes l)

    ownerHeader = prop >>= owner >>= (pure . translateX (nodeW/2 -1) . rightText)
    trustHeader = translateX (-nodeW/2+1) <$> trustHeader' leftText

    trustHeader' txt = case t of
                          _  | PTrust `notElem` attrs -> Nothing
                          t' | t' == defaultTrust -> Nothing
                          t' | t' == 0 -> Just $ txt "impossible"
                          _  -> Just $ txt ("trust = " ++ percentageText (getTrust t))

    displayCost c'
      | c' == defaultCost   = mempty
      | otherwise = rightText $ "(" ++ printf "%.1f" (getCost c') ++ ")"
    displayProgress p'
      | p' == defaultProgress = mempty
      | p' == 1 = leftText "done"
      | otherwise = leftText $ percentageText $ getProgress p'

    -- color is red if the project hasn't started, green if it's done, or yellow
    -- otherwise (i.e.  in progress)
    projColor =
      if colorByP then
              (if p == 0 then pink else if p == 1 then lightgreen else lightyellow)
         else white

    percentageText pct = printf "%.1f%%" (pct * 100)
