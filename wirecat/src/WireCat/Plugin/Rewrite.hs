{-# LANGUAGE DeriveFunctor #-}

module WireCat.Plugin.Rewrite where

import Control.Monad (ap)

data Rewrite a
  = NoRewrite
  | Rewrite a
  | Error String
  deriving (Functor)

instance Semigroup (Rewrite a) where
  NoRewrite <> x = x
  x <> _ = x

instance Monoid (Rewrite a) where
  mempty = NoRewrite

instance Applicative Rewrite where
  pure = Rewrite
  (<*>) = ap

instance Monad Rewrite where
  NoRewrite >>= _ = NoRewrite
  Rewrite a >>= k = k a
  Error err >>= _ = Error err
