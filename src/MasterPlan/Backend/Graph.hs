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

import           Control.Applicative         ((<|>))
import           Control.Monad.State
import           Data.List                   (intersperse, isSuffixOf)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Tree
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

-- * Types

data NodeType = SumNode | ProductNode | SequenceNode | AtomicNode

-- |Data type used by the tree
data PNode = PNode (Maybe ProjectKey)
                 (Maybe ProjectProperties)
                 Cost
                 Trust
                 Progress
          | NodeRef ProjectKey

type RenderModel = Tree (NodeType, PNode)

mkLeaf :: PNode -> RenderModel
mkLeaf a = Node (AtomicNode, a) []

-- |Translates a ProjectSystem into a Tree PNode
toRenderModel :: ProjectSystem -> ProjectKey -> State [ProjectKey] (Maybe RenderModel)
toRenderModel sys rootK = case M.lookup rootK (bindings sys) of
                            Nothing -> pure Nothing
                            Just b  -> Just <$> bindingToRM rootK b
  where
    bindingToRM :: ProjectKey -> Binding -> State [ProjectKey] RenderModel
    bindingToRM key (BindingExpr prop p) = projToRM p (Just key) (Just prop)
    bindingToRM key (BindingAtomic prop c t p) = pure $ mkLeaf $ PNode (Just key)
                                                                       (Just prop)
                                                                       c t p

    mkNode :: (PNode -> [RenderModel] -> RenderModel)
           -> ProjectExpr
           -> NE.NonEmpty ProjectExpr
           -> Maybe ProjectKey
           -> Maybe ProjectProperties
           -> State [ProjectKey] RenderModel
    mkNode f p ps key prop = f (PNode key prop
                                     (cost sys p)
                                     (trust sys p)
                                     (progress sys p))
                               <$> mapM (\p' -> projToRM p' Nothing Nothing) (NE.toList ps)

    projToRM :: ProjectExpr -> Maybe ProjectKey -> Maybe ProjectProperties -> State [ProjectKey] RenderModel
    projToRM p@(Sum ps) = mkNode (\x -> Node (SumNode, x)) p ps
    projToRM p@(Sequence ps) = mkNode (\x -> Node (SequenceNode, x)) p ps
    projToRM p@(Product ps) = mkNode (\x -> Node (ProductNode, x)) p ps
    projToRM (Reference n) =
      \k p -> case M.lookup n $ bindings sys of
                Nothing -> pure $ Node (AtomicNode, PNode k (p <|> pure defaultProjectProps {title=getProjectKey n}) defaultCost defaultTrust defaultProgress) []
                Just b -> do alreadyProcessed <- gets (n `elem`)
                             if alreadyProcessed
                               then pure $ Node (AtomicNode, NodeRef $ ProjectKey $ bindingTitle b) []
                               else modify (n:) >> bindingToRM n b

-- |how many leaf nodes
leafCount :: Tree a -> Double
leafCount (Node _ []) = 1
leafCount (Node _ ts) = sum $ leafCount <$> ts

-- |Options for rendering
data RenderOptions = RenderOptions { colorByProgress  :: Bool -- ^Whether to color boxes depending on progress
                                   , renderWidth      :: Integer -- ^The width of the output image
                                   , renderHeight     :: Integer -- ^The height of the output image
                                   , rootKey          :: ProjectKey -- ^The name of the root project
                                   , whitelistedProps :: [ProjAttribute] -- ^Properties that should be rendered
                                   } deriving (Eq, Show)

-- | The main rendering function
render ∷ FilePath -> RenderOptions-> ProjectSystem → IO ()
render fp (RenderOptions colorByP w h rootK props) sys =
  let noRootEroor = texterific $ "no project named \"" ++ getProjectKey rootK ++ "\" found."
      dia = fromMaybe noRootEroor $ renderTree colorByP props <$> evalState (toRenderModel sys rootK) []
  in renderRasterific fp (dims2D (fromInteger w) (fromInteger h)) $ bgFrame 1 white $ centerXY dia

-- |Render a multi-line text to file
renderText ∷ FilePath -> RenderOptions-> [String] → IO ()
renderText fp RenderOptions { renderWidth=w, renderHeight=h } ss =
  let dia = multilineText (0.1 :: Float) ss
  in renderRasterific fp (dims2D (fromInteger w) (fromInteger h)) $ bgFrame 1 white $ centerXY dia

renderTree :: Bool -> [ProjAttribute] -> RenderModel -> QDiagram B V2 Double Any
renderTree colorByP props (Node (_, n) [])    = alignL $ renderNode colorByP props n
renderTree colorByP props x@(Node (ty, n) ts@(t:_)) =
    (strutY (12 * leafCount x) <> alignL (centerY $ renderNode colorByP props n))
    |||  (translateX 2 typeSymbol # withEnvelope (mempty :: D V2 Double) <> hrule 4 # lwO 2)
    |||  centerY (headBar === treeBar sizes)
    |||  centerY (vcat $ map renderSubTree ts)
  where
    sizes = map ((* 6) . leafCount) ts
    renderSubTree subtree = hrule 4 # lwO 2 ||| renderTree colorByP props subtree

    headBar = strutY $ leafCount t * 6

    treeBar :: [Double] -> QDiagram B V2 Double Any
    treeBar (s1:s2:ss) = vrule s1 # lwO 2 === vrule s2 # lwO 2 === treeBar (s2:ss)
    treeBar [s1] = strutY s1
    treeBar _ = mempty

    typeSymbol =
      let txt = case ty of
                    SumNode      -> text "+"
                    ProductNode  -> text "x"
                    SequenceNode -> text "->"
                    AtomicNode   -> mempty
      in txt # fontSizeL 2 # bold <> roundedRect 3 2 1 # fc white # lwO 1

renderNode :: Bool -> [ProjAttribute] -> PNode -> QDiagram B V2 Double Any
renderNode _        _     (NodeRef (ProjectKey n)) =
   text n <> roundedRect 30 12 0.5 # lwO 2 # fc white # dashingN [0.005, 0.005] 0
renderNode colorByP props (PNode _   prop c t p) =
   centerY nodeDia <> strutY 12
  where
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
    givenProp pro x = if pro `elem` props then x else Nothing

    headerSection = case [progressHeader, titleHeader, costHeader] of
                        [Nothing, Nothing, Nothing] -> Nothing
                        l -> Just $ strutY 2 <> strutX nodeW <> mconcat (catMaybes l)
    progressHeader = givenProp PProgress $ Just $ displayProgress p # translateX (-nodeW/2 + 1)
    titleHeader = givenProp PTitle $
                    (centerXY . textOverflow' FontSlantNormal FontWeightBold 1 30 0.1 . title) <$> prop
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
                          _  | PTrust `notElem` props -> Nothing
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
