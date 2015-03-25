module Graphics.CheeseGrater.TestPointListPoly (unitTests) where

import           Test.Tasty                          (TestTree, testGroup)
import           Test.Tasty.HUnit                    (assertBool, testCase)
-- import           Test.Tasty.QuickCheck               (testProperty)

import           Graphics.CheeseGrater.ApproxEq      (ApproxEq ((~==)))
import           Graphics.CheeseGrater.PointListPoly (PointListPoly (..), editPlp)
import           Graphics.CheeseGrater.VecMath       (Point (..))

------------------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "PointListPoly"
            [ testApproxEq
            , testEditPlp
            ]

------------------------------------------------------------

testApproxEq :: TestTree
testApproxEq = testGroup "equality of PointListPoly"
               [ testCase "polygons are equal"     $ assertBool "(ad-hoc)" eq
               , testCase "polygons are not equal" $ assertBool "(ad-hoc)" ne
               ]
  where
    eq, ne :: Bool
    eq =       PointListPoly [a, b, c, d, e, f, g] ~== PointListPoly [e, f, g, a, b, c, d]
    ne = not $ PointListPoly [a, b, c, d, e, f, g] ~== PointListPoly [a, b, c, d, e, f]
    a, b, c, d, e, f, g :: Point
    a = Point   0 0
    b = Point   0 1
    c = Point   0 2
    d = Point 0.5 3
    e = Point   1 2
    f = Point   1 1
    g = Point   1 0

testEditPlp :: TestTree
testEditPlp = testGroup "editing of PointListPoly"
              [ testCase "within-chain edit" $ assertBool "(ad-hoc)" wc
              , testCase "end-wrap edit"     $ assertBool "(ad-hoc)" ew
              ]
  where
    wc, ew :: Bool
    wc = editPlp p (2, c') (5, f') ~== PointListPoly [  a, b, c', f', g  ]
    ew = editPlp p (5, f') (1, b') ~== PointListPoly [ b', c,  d,  e, f' ]
    p :: PointListPoly
    p = PointListPoly [a, b, c, d, e, f, g]
    a, b, c, d, e, f, g, b', c', f' :: Point
    a  = Point   0 0.5
    b  = Point   0 1.5
    c  = Point   0 2.5
    d  = Point 0.5 3.5
    e  = Point   1 2.5
    f  = Point   1 1.5
    g  = Point   1 0.5
    b' = Point   0   1
    c' = Point   0   3
    f' = Point   1   2
