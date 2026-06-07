{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module WireCat.Label
  ( Label (..),
    ToLabel (..),
    toLabel,
    labelizeFree,
  )
where

import WireCat.Free (Free (..))

data Label a b = Label
  { operationLabel :: String
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
    { operationLabel = toLabelName op
    }

labelizeFree :: (ToLabel op) => Free op r s -> Free Label r s
labelizeFree IdentityR = IdentityR
labelizeFree (LiftR op) = LiftR (toLabel op)
labelizeFree (NamedR name location inner) =
  NamedR name location (labelizeFree inner)
labelizeFree (ComposeR left right) = ComposeR (labelizeFree left) (labelizeFree right)
labelizeFree ProjectR = ProjectR
labelizeFree (CombineR left right) = CombineR (labelizeFree left) (labelizeFree right)
labelizeFree (RelabelR old new) = RelabelR old new
