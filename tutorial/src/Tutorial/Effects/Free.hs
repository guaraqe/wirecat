{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Tutorial.Effects.Free where

import Control.Category (Category (..), (>>>))
import Prelude hiding ((.))

data ReadWrite a b where
  ReadLine :: ReadWrite () String
  WriteLine :: ReadWrite String ()

data Free eff a b where
  Id :: Free eff a a
  Comp :: Free eff a b -> Free eff b c -> Free eff a c
  Fork :: Free eff a b -> Free eff a c -> Free eff a (b, c)
  Exl :: Free eff (a, b) a
  Exr :: Free eff (a, b) b
  Terminal :: Free eff a ()
  Embed :: eff a b -> Free eff a b

class (Category cat) => Cartesian cat where
  exl :: cat (a, b) a
  exr :: cat (a, b) b
  fork :: cat a b -> cat a c -> cat a (b, c)
  terminal :: cat a ()

instance Category (Free eff) where
  id = Id
  a . b = Comp b a

inject :: eff a b -> Free eff a b
inject = Embed

instance Cartesian (Free eff) where
  exl = Exl
  exr = Exr
  fork = Fork
  terminal = Terminal

run ::
  (Monad m) =>
  (forall x y. eff x y -> x -> m y) ->
  Free eff a b ->
  a ->
  m b
run f cat a = case cat of
  Id -> pure a
  Comp cat1 cat2 -> do
    b <- run f cat1 a
    run f cat2 b
  Fork cat1 cat2 ->
    (,) <$> run f cat1 a <*> run f cat2 a
  Exl -> case a of
    (x, _) -> pure x
  Exr -> case a of
    (_, y) -> pure y
  Terminal -> pure ()
  Embed eff -> f eff a

pipeline :: Free ReadWrite () ()
pipeline =
  inject ReadLine >>> inject WriteLine
