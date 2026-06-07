{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=WireCat #-}

module WireCat.Test.Records where

import Data.Functor.Identity (Identity (..))
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Row.Internal (Empty)
import Data.Row.Records hiding (compose)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import WireCat.Free
import WireCat.Graph
import WireCat.Label
import WireCat.Records

data Operation a b where
  MakeX :: Operation (Rec Empty) (Rec ("x" .== Int))
  MakeY :: Operation (Rec Empty) (Rec ("y" .== Bool))

deriving instance Show (Operation a b)

instance ToLabel Operation

interpretOperation :: Operation (Rec r) (Rec s) -> KleisliRec Identity r s
interpretOperation MakeX = KleisliRec $ \_ -> Identity (#x .== 42)
interpretOperation MakeY = KleisliRec $ \_ -> Identity (#y .== True)

program :: Free Operation Empty ("x" .== Int)
program =
  project
    `compose` combine
      (inject MakeX)
      (inject MakeY)

punnedHandler ::
  KleisliRec Identity ("x" .== Int) ("x" .== Int)
punnedHandler = KleisliRec $ \R {x} -> Identity R {x}

explicitHandler ::
  KleisliRec Identity ("x" .== Int) ("x" .== Int)
explicitHandler = KleisliRec $ \R {x = input} -> Identity R {x = input + 1}

namedSubpipeline :: Free Operation Empty ("x" .== Int)
namedSubpipeline = proc R {} -> inject MakeX -< R {}

namedPipeline :: Free Operation Empty ("x" .== Int)
namedPipeline = proc R {} -> namedSubpipeline -< R {}

pipelineWithLocal :: Free Operation Empty ("x" .== Int)
pipelineWithLocal =
  let localSubpipeline :: Free Operation Empty ("x" .== Int)
      localSubpipeline = proc R {} -> inject MakeX -< R {}
   in proc R {} -> localSubpipeline -< R {}

tests :: TestTree
tests =
  testGroup
    "Records"
    [ testCase "free record category interpretation" $
        let KleisliRec f = foldFree interpretOperation program
         in runIdentity (f empty) @?= (#x .== 42),
      testCase "punned R handler syntax" $
        let KleisliRec f = punnedHandler
         in runIdentity (f (#x .== 42)) @?= (#x .== 42),
      testCase "explicit R handler syntax" $
        let KleisliRec f = explicitHandler
         in runIdentity (f (#x .== 42)) @?= (#x .== 43),
      testCase "extracts named top-level subpipelines" $ do
        let graphs = toGraphs namedPipeline
            root = graphs Map.! "namedPipeline"
        Map.keys graphs @?= ["namedPipeline", "namedSubpipeline"]
        assertBool
          "root graph references the named subpipeline"
          (any ((== Just "namedSubpipeline") . subgraph) (nodes root))
        assertBool
          "subpipeline includes its source location"
          (maybe False (not . null) (location (graphs Map.! "namedSubpipeline"))),
      testCase "extracts named local subpipelines" $ do
        let graphs = toGraphs pipelineWithLocal
        Map.keys graphs @?= ["localSubpipeline", "main"],
      testCase "connects pipeline input and output boundaries" $ do
        let graph =
              toGraph
                (identity :: Free Operation ("x" .== Int) ("x" .== Int))
            inputNode = find ((== Just InputBoundary) . boundary) (nodes graph)
            outputNode = find ((== Just OutputBoundary) . boundary) (nodes graph)
        case (inputNode, outputNode) of
          (Just inputNodeDef, Just outputNodeDef) -> do
            name inputNodeDef @?= "input"
            name outputNodeDef @?= "output"
            edges graph
              @?= [ Edge
                      Plug {node = nodeId inputNodeDef, attr = "x"}
                      Plug {node = nodeId outputNodeDef, attr = "x"}
                  ]
          _ -> assertFailure "missing input or output boundary node",
      testCase "omits empty pipeline boundaries" $ do
        let graph = toGraph (identity :: Free Operation Empty Empty)
        assertBool
          "empty graph has no boundary nodes"
          (all ((== Nothing) . boundary) (nodes graph))
    ]
