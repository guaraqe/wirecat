{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module WireCat.Records
  ( RecordCategory (..),
    KleisliRec (..),
    Interpret (..),
    pickField,
    (:>),
  )
where

import Data.Row.Dictionaries (FreeForall)
import Data.Row.Records hiding (compose)
import qualified Data.Row.Records as Row
import Type.Reflection (Typeable)
import Prelude hiding (id)

class RecordCategory cat where
  identity :: cat r r

  compose :: cat s t -> cat r s -> cat r t

  project ::
    (Subset r s, FreeForall r, Forall r Typeable, Forall s Typeable) =>
    cat s r

  combine ::
    (Forall r Typeable, Forall s Typeable, Forall (r .// s) Typeable) =>
    cat a r ->
    cat a s ->
    cat a (r .// s)

  relabel ::
    forall old new r.
    ( KnownSymbol old,
      KnownSymbol new,
      Forall r Typeable,
      Forall (Rename old new r) Typeable
    ) =>
    Label old ->
    Label new ->
    cat r (Rename old new r)

newtype KleisliRec m r s = KleisliRec
  { runKleisliRec :: Rec r -> m (Rec s)
  }

instance (Monad m) => RecordCategory (KleisliRec m) where
  identity = KleisliRec pure

  compose (KleisliRec f) (KleisliRec g) =
    KleisliRec $ \x -> g x >>= f

  project = KleisliRec $ \r -> pure (restrict r)

  combine (KleisliRec f) (KleisliRec g) =
    KleisliRec $ \x -> do
      r <- f x
      s <- g x
      pure (r .// s)

  relabel old new = KleisliRec $ \x -> pure (Row.rename old new x)

class (RecordCategory cat) => Interpret cat eff where
  interpret ::
    (Forall a Typeable, Forall b Typeable) =>
    eff (Rec a) (Rec b) ->
    cat a b

type eff :> cat = Interpret cat eff

-- | Project a single field out of the context row.
--
-- The target row is fully determined by the label and the input row,
-- which avoids the ambiguity of bare 'project' when combining several
-- single-field projections into a record.
pickField ::
  forall l r cat.
  ( RecordCategory cat,
    KnownSymbol l,
    Subset (l .== (r .! l)) r,
    FreeForall (l .== (r .! l)),
    Forall (l .== (r .! l)) Typeable,
    Forall r Typeable
  ) =>
  Label l ->
  cat r (l .== (r .! l))
pickField _ = project
