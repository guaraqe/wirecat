{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module WireCat.TH
  ( EffectStep,
    defineEffect,
    step,
  )
where

import Data.Char (isAlpha, toLower)
import Data.Row.Records (Rec)
import Language.Haskell.TH
import WireCat.Label (ToLabel)
import WireCat.Records (Interpret (interpret))

data EffectStep = EffectStep String TypeQ TypeQ

step :: String -> TypeQ -> TypeQ -> EffectStep
step = EffectStep

defineEffect :: String -> [EffectStep] -> Q [Dec]
defineEffect effectName steps = do
  let effect = mkName effectName
  a <- newName "a"
  b <- newName "b"
  constructors <- traverse (mkConstructor effect) steps
  helpers <- concat <$> traverse (mkHelper effect) steps
  pure $
    [ DataD
        []
        effect
        [PlainTV a BndrReq, PlainTV b BndrReq]
        Nothing
        constructors
        [],
      StandaloneDerivD
        Nothing
        []
        (AppT (ConT ''Show) (AppT (AppT (ConT effect) (VarT a)) (VarT b))),
      InstanceD Nothing [] (AppT (ConT ''ToLabel) (ConT effect)) []
    ]
      <> helpers

mkConstructor :: Name -> EffectStep -> Q Con
mkConstructor effect (EffectStep constructorName inputQ outputQ) = do
  input <- inputQ
  output <- outputQ
  pure $
    GadtC
      [mkName constructorName]
      []
      (AppT (AppT (ConT effect) (AppT (ConT ''Rec) input)) (AppT (ConT ''Rec) output))

mkHelper :: Name -> EffectStep -> Q [Dec]
mkHelper effect (EffectStep constructorName inputQ outputQ) = do
  input <- inputQ
  output <- outputQ
  cat <- newName "cat"
  let helperName = mkName (lowerConstructor constructorName)
      helperType =
        ForallT
          [PlainTV cat SpecifiedSpec]
          [AppT (AppT (ConT ''Interpret) (VarT cat)) (ConT effect)]
          (AppT (AppT (VarT cat) input) output)
  pure
    [ SigD helperName helperType,
      ValD
        (VarP helperName)
        (NormalB (AppE (VarE 'interpret) (ConE (mkName constructorName))))
        []
    ]

lowerConstructor :: String -> String
lowerConstructor [] = []
lowerConstructor (c : cs)
  | isAlpha c = toLower c : cs
  | otherwise = c : cs
