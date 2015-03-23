module Graphics.CheeseGrater.TestVecMath
       ( unitTests
       ) where

import           Data.AEq                      (AEq (..))
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.QuickCheck         (testProperty)

import           Graphics.CheeseGrater.VecMath (Vec2 (..), vAdd, vSub)

------------------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "VecMath"
           [ testAddSub
           ]

------------------------------------------------------------

testAddSub :: TestTree
testAddSub = testProperty "addition and subtraction cancel" addSub
  where
    addSub :: Vec2 -> Vec2 -> Bool
    addSub v1 v2 = (v1 `vAdd` v2) `vSub` v2 ~== v1

