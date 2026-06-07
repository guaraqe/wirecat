{-# LANGUAGE RecordWildCards #-}

module WireCat.Plugin (plugin) where

import qualified Data.Generics as SYB
import GHC.Hs (HsParsedModule (..))
import qualified GHC.Plugins as Plugins
import WireCat.Plugin.Records
  ( transformHandlerMatch,
    transformNamedBinding,
    transformRecordExpr,
    transformRecords,
  )

plugin :: Plugins.Plugin
plugin =
  Plugins.defaultPlugin
    { Plugins.parsedResultAction = parsedAction,
      Plugins.pluginRecompile = Plugins.purePlugin
    }

parsedAction ::
  [Plugins.CommandLineOption] ->
  Plugins.ModSummary ->
  Plugins.ParsedResult ->
  Plugins.Hsc Plugins.ParsedResult
parsedAction _args _modSum result = do
  let parsed = Plugins.parsedResultModule result
      hsmod = hpm_module parsed
      named = SYB.everywhere (SYB.mkT transformNamedBinding) hsmod
      procRewritten = SYB.everywhere (SYB.mkT transformRecords) named
      patternsRewritten =
        SYB.everywhere (SYB.mkT transformHandlerMatch) procRewritten
      hsmod' = SYB.everywhere (SYB.mkT transformRecordExpr) patternsRewritten
  pure result {Plugins.parsedResultModule = parsed {hpm_module = hsmod'}}
