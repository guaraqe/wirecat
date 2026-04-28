{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module WireCat.Test.Dot where

import qualified Data.GraphViz.Attributes.Complete as DotAttr
import Data.List (isInfixOf)
import Data.Row.Internal (Empty)
import Data.Row.Records hiding (compose)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import WireCat.Dot
import WireCat.Free
import WireCat.Graph
import WireCat.Label
import WireCat.Records

data Operation a b where
  Cast :: Operation (Rec Empty) (Rec ("casted" .== ()))
  Neg :: Operation (Rec ("casted" .== ())) (Rec Empty)

deriving instance Show (Operation a b)

instance ToLabel Operation

tests :: TestTree
tests =
  testGroup
    "Dot"
    [ testCase "renders top-to-bottom DOT from proc graph" $ do
        let diagram =
              renderDot $
                toGraph $
                  inject Neg
                    `compose` project
                    `compose` inject Cast
        assertBool "contains digraph header" ("digraph" `isInfixOf` diagram)
        assertBool "uses top-to-bottom layout" ("rankdir=TB" `isInfixOf` diagram)
        assertBool "contains source node label" ("Cast" `isInfixOf` diagram)
        assertBool "contains target node label" ("Neg" `isInfixOf` diagram)
        assertBool "contains record output port" ("out_casted" `isInfixOf` diagram)
        assertBool "contains record input port" ("in_casted" `isInfixOf` diagram)
        assertBool "contains connection metadata" ("tailport" `isInfixOf` diagram),
      testCase "supports custom rank direction" $ do
        let diagram =
              renderDotWith
                defaultDotOptions
                  { rankDir = DotAttr.FromLeft
                  }
                (toGraph (identity :: Free Operation Empty Empty))
        assertBool "uses custom left-to-right layout" ("rankdir=LR" `isInfixOf` diagram)
    ]
