module Main where

import           Test.Tasty                        (TestTree, defaultMain,
                                                    testGroup)

import qualified Graphics.CheeseGrater.TestVecMath as TestVecMath (unitTests)

------------------------------------------------------------

main :: IO ()
main = defaultMain tests

------------------------------------------------------------

tests :: TestTree
tests = testGroup "Tests"
        [ TestVecMath.unitTests
        ]
