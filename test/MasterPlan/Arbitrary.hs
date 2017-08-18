{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module MasterPlan.Arbitrary () where

import qualified Data.List.NonEmpty        as NE
import           Data.Maybe                (isNothing)
import           MasterPlan.Data
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary ProjectProperties where

  arbitrary =
      let s = getASCIIString <$> arbitrary
          maybeStr x = if null x then Nothing else Just x
          os = oneof [pure Nothing, maybeStr <$> s]
          titleG = Just <$> elements [[x] | x <- ['a'..'z']]
      in ProjectProperties <$> titleG <*> os <*> os <*> os

  shrink p = (if isNothing (description p) then [] else [p { description = Nothing }]) ++
             (if isNothing (url p) then [] else [p { url = Nothing }]) ++
             (if isNothing (owner p) then [] else [p { owner = Nothing }])

instance Arbitrary (Project a) where

  arbitrary =
    let shrinkFactor n = 3 * n `quot` 5
        unitGen = elements [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
    in  frequency [ (1, Sum defaultProjectProps <$> scale shrinkFactor arbitrary)
                  , (1, Product defaultProjectProps <$> scale shrinkFactor arbitrary)
                  , (1, Sequence defaultProjectProps <$> scale shrinkFactor arbitrary)
                  , (2, Atomic <$> arbitrary
                               <*> (Cost <$> elements [0, 1 .. 100])
                               <*> (Trust <$> unitGen)
                               <*> (Progress <$> unitGen)) ]

  shrink (Sum _ ps)      = NE.toList ps
  shrink (Product _ ps)  = NE.toList ps
  shrink (Sequence _ ps) = NE.toList ps
  shrink _               = []
