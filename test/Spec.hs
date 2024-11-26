module Main where

import Test.Hspec
import qualified ChibiHashSpec

main :: IO ()
main = hspec ChibiHashSpec.spec