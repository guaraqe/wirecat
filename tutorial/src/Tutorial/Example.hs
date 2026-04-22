{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Tutorial.Example where

import Data.Row.Records
import Tutorial.Category qualified as Category
import Tutorial.Label (ToLabel (..))
import Tutorial.Mermaid qualified as Mermaid

data Operation a b where
  Increase :: Operation (Rec ("value" .== Int)) (Rec ("value" .== Int))
  Decrease :: Operation (Rec ("value" .== Int)) (Rec ("value" .== Int))

deriving instance Show (Operation a b)

instance ToLabel Operation

categoryOperation :: Category.Free Operation ("value" .== Int) ("value" .== Int)
categoryOperation =
  Category.inject Increase
    `Category.compose` Category.inject Decrease
    `Category.compose` Category.inject Increase

main :: IO ()
main = do
  let basename = "tmp/category-operation"
      mermaid = Category.toMermaid categoryOperation
  Mermaid.renderMermaidWithMmdc basename mermaid
