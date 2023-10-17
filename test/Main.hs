module Main where

import Test.Hspec.Runner (
    hspec,
 )

import Spec (
    spec,
 )
import Test.Hspec (parallel)

main :: IO ()
main = hspec . parallel $ spec
