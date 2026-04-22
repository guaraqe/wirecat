{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module WireCat.Test.Records where

import WireCat.Free
import WireCat.Records
import Data.Functor.Identity (Identity (..))
import Data.Row.Internal (Empty)
import Data.Row.Records hiding (compose)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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

tests :: TestTree
tests =
  testGroup
    "Records"
    [ testCase "free record category interpretation" $
        let KleisliRec f = foldFree interpretOperation program
         in runIdentity (f empty) @?= (#x .== 42)
    ]
