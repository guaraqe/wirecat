{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module WireCat.Free
  ( Free (..),
    inject,
    hoist,
    foldFree,
  )
where

import WireCat.Records
  ( Interpret (..),
    RecordCategory (..),
  )
import Data.Row.Dictionaries (FreeForall)
import Data.Row.Records hiding (compose)
import Type.Reflection (Typeable)
import Prelude hiding (id)

data Free op r s where
  IdentityR :: Free op r r
  LiftR ::
    (Forall r Typeable, Forall s Typeable) =>
    op (Rec r) (Rec s) ->
    Free op r s
  ComposeR :: Free op s t -> Free op r s -> Free op r t
  ProjectR ::
    (Subset r s, FreeForall r, Forall r Typeable, Forall s Typeable) =>
    Free op s r
  CombineR ::
    (Forall r Typeable, Forall s Typeable, Forall (r .// s) Typeable) =>
    Free op a r ->
    Free op a s ->
    Free op a (r .// s)
  RelabelR ::
    ( KnownSymbol old,
      KnownSymbol new,
      Forall r Typeable,
      Forall (Rename old new r) Typeable
    ) =>
    Label old ->
    Label new ->
    Free op r (Rename old new r)

inject ::
  (Forall r Typeable, Forall s Typeable) =>
  op (Rec r) (Rec s) ->
  Free op r s
inject = LiftR

hoist ::
  (forall x y. op x y -> op' x y) ->
  Free op r s ->
  Free op' r s
hoist _ IdentityR = IdentityR
hoist f (LiftR op) = LiftR (f op)
hoist f (ComposeR left right) =
  ComposeR (hoist f left) (hoist f right)
hoist _ ProjectR = ProjectR
hoist f (CombineR left right) =
  CombineR (hoist f left) (hoist f right)
hoist _ (RelabelR old new) = RelabelR old new

foldFree ::
  (RecordCategory cat) =>
  (forall x y. op (Rec x) (Rec y) -> cat x y) ->
  Free op r s ->
  cat r s
foldFree _ IdentityR = identity
foldFree f (LiftR op) = f op
foldFree f (ComposeR left right) =
  compose (foldFree f left) (foldFree f right)
foldFree _ ProjectR = project
foldFree f (CombineR left right) =
  combine (foldFree f left) (foldFree f right)
foldFree _ (RelabelR old new) = relabel old new

instance RecordCategory (Free op) where
  identity = IdentityR
  compose = ComposeR
  project = ProjectR
  combine = CombineR
  relabel = RelabelR

instance Interpret (Free eff) eff where
  interpret = inject
