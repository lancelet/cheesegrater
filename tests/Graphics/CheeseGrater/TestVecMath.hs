module Graphics.CheeseGrater.TestVecMath (unitTests) where

import           Data.Maybe                     (isNothing)
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               (assertBool, testCase)
import           Test.Tasty.QuickCheck          (choose, forAll, testProperty)

import           Graphics.CheeseGrater.ApproxEq (ApproxEq ((~==)))
import           Graphics.CheeseGrater.VecMath  (Line (..), Point (..),
                                                 Vec (..), intersectLines,
                                                 intersectLinesQuick, pAdd,
                                                 pSubP, pSubV, vAdd, vAngle,
                                                 vDot, vLength, vMul, vSub)

------------------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "VecMath"
           [ testAddSubVec
           , testDot
           , testAddSubPoint
           , testAddSubPointV
           , testVMul
           , testVLength
           , testVAngle
           , testIntersectLines
           , testIntersectLinesQuick
           ]

------------------------------------------------------------

testAddSubVec :: TestTree
testAddSubVec = testProperty "inverse relationship of addition and subtraction for Vec" addSub
  where
    addSub :: Vec -> Vec -> Bool
    addSub v1 v2 = (v1 `vAdd` v2) `vSub` v2 ~== v1

testDot :: TestTree
testDot = testCase "dot product" $ assertBool "(ad-hoc)" f
  where
    f :: Bool
    f = Vec 5 3 `vDot` Vec 2 6 ~== 28

testAddSubPoint :: TestTree
testAddSubPoint = testProperty "inverse relationship of addition and subtraction for Point" addSub
  where
    addSub :: Point -> Vec -> Bool
    addSub p v = (p `pAdd` v) `pSubP` p ~== v

testAddSubPointV :: TestTree
testAddSubPointV = testProperty "inverse relationship of addition and subtraction for Point wrt Vec" addSub
  where
    addSub :: Point -> Vec -> Bool
    addSub p v = (p `pAdd` v) `pSubV` v ~== p

testVMul :: TestTree
testVMul = testProperty
           "inverse relationship of multiplication and division for Vec" $
           forAll (choose (1,100)) mulInv
  where
    mulInv :: Float -> Vec -> Bool
    mulInv c v = (1.0 / c) `vMul` (c `vMul` v) ~== v

testVLength :: TestTree
testVLength = testProperty "vector length should be the square root of the vector dot with itself" l
  where
    l :: Vec -> Bool
    l v = sqrt (v `vDot` v) ~== vLength v

testVAngle :: TestTree
testVAngle = testGroup "angle between vectors"
             [ testCase "positive angle" $ assertBool "(ad-hoc)" f
             , testCase "negative angle" $ assertBool "(ad-hoc)" g
             ]
  where
    f, g :: Bool
    f = Vec 10 0 `vAngle` Vec  5 5 ~==  a
    g = Vec  5 5 `vAngle` Vec 10 0 ~== -a
    a :: Float
    a = 45.0 * pi / 180.0

testIntersectLines :: TestTree
testIntersectLines = testCase "parallel lines should return no intersection" $ assertBool "(ad-hoc)" f
  where
    f :: Bool
    f = isNothing (intersectLines 0.01 l1 l2)
    l1, l2 :: Line
    l1 = Line (Point 0 0) (Vec 1 1)
    l2 = Line (Point 5 0) (Vec 1 1)

testIntersectLinesQuick :: TestTree
testIntersectLinesQuick = testCase "line-line intersection" $ assertBool "(ad-hoc)" f
  where
    f :: Bool
    f = intersectLinesQuick l1 l2 ~== Point 2 2
    l1, l2 :: Line
    l1 = Line (Point 1 1) (Vec   1  1)
    l2 = Line (Point 3 1) (Vec (-2) 2)
