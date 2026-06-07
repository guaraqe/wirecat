{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=WireCat #-}

module WireCat.Test.Records where

import Data.Functor.Identity (Identity (..))
import Data.Row.Internal (Empty)
import Data.Row.Records hiding (compose)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import WireCat.Free
import WireCat.Records

data Operation a b where
  MakeX :: Operation (Rec Empty) (Rec ("x" .== Int))
  MakeY :: Operation (Rec Empty) (Rec ("y" .== Bool))

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
         in runIdentity (f (#x .== 42)) @?= (#x .== 43)
    ]
