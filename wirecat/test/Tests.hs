module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified WireCat.Test.Dot as Dot
import qualified WireCat.Test.Records as Records

main :: IO ()
main = defaultMain $ testGroup "Tests" [Dot.tests, Records.tests]
