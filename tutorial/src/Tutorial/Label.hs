{-# LANGUAGE DefaultSignatures #-}

module Tutorial.Label
  ( Label (..),
    ToLabel (..),
    toLabel,
  )
where

data Label a b = Label
  { inputLabel :: String,
    operationLabel :: String,
    outputLabel :: String
  }

class ToLabel op where
  toLabelName :: op a b -> String
  default toLabelName :: (Show (op a b)) => op a b -> String
  toLabelName = show

toLabel ::
  (ToLabel op) =>
  op a b -> Label a b
toLabel op =
  Label
    { inputLabel = "",
      operationLabel = toLabelName op,
      outputLabel = ""
    }
