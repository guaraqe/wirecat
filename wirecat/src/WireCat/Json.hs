{-# LANGUAGE OverloadedStrings #-}

module WireCat.Json
  ( writeJsonFile,
    encodeGraphs,
  )
where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

encodeGraphs :: Map String Value -> Value
encodeGraphs graphs =
  object ["graphs" .= object [Key.fromString name .= v | (name, v) <- Map.toList graphs]]

writeJsonFile :: FilePath -> Map String Value -> IO ()
writeJsonFile path = BL.writeFile path . Aeson.encode . encodeGraphs
