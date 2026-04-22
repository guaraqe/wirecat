{-# LANGUAGE RecordWildCards #-}

module WireCat.Plugin (plugin) where

import WireCat.Plugin.Records (transformRecords)
import qualified Data.Generics as SYB
import GHC.Hs (HsParsedModule (..))
import qualified GHC.Plugins as Plugins

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
      hsmod' = SYB.everywhere (SYB.mkT transformRecords) hsmod
  pure result {Plugins.parsedResultModule = parsed {hpm_module = hsmod'}}
