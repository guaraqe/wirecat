module Main (main) where

import qualified WireCat.Test.Dot as Dot
import qualified WireCat.Test.Records as Records
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Tests" [Dot.tests, Records.tests]
