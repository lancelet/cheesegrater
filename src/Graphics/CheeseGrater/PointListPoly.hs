module Graphics.CheeseGrater.PointListPoly (
  -- * Types
    PointListPoly(..)
  -- * Functions
  , editPlp
  ) where

import           Graphics.CheeseGrater.ApproxEq (ApproxEq (..))
import           Graphics.CheeseGrater.VecMath  (Point (..), Vec(..), vLength)

------------------------------------------------------------

-- | Polygon, specified by a list of points that form the close, non-crossing boundary.
data PointListPoly = PointListPoly [Point]
                   deriving (Show)

-- | Test for approximate equality of two 'PointListPoly's.
-- This test has to account for the fact that two polygons are equivalent if they have the same
-- sequence of points, but that the points may not necessarily start at the same point in the
-- list. Thus, the points have to be "caterpillared" to check if there is a corresponding match.
instance ApproxEq PointListPoly where

  approxEq t (PointListPoly pas) (PointListPoly pbs) = any (aeps pas) (caterpillar pbs)
    where
      aeps :: [Point] -> [Point] -> Bool
      aeps []     []     = True
      aeps _      []     = False
      aeps []     _      = False
      aeps (a:as) (b:bs) = approxEq t a b && aeps as bs

  absoluteTolFromRelativeTol t (PointListPoly pas) (PointListPoly pbs) =
    maximum $ map ptTol $ pas ++ pbs
    where
      ptTol :: Point -> Float
      ptTol (Point x y) = max t (t * vLength (Vec x y))

-- | Edits a PointListPoly.
-- Modifies a `PointListPoly` by removing a contiguous chain of points and adding a linear segment.
-- For example,
--  p = PointListPoly [ a, b, c, d, e, f, g ]
--  editPlp p (2, c') (5, f') = PointListPoly [  a, b, c', f', g  ]  -- within-chain example
--  editPlp p (5, f') (1, b') = PointListPoly [ b', c,  d,  e, f' ]  -- end-wrap-chain example
editPlp :: PointListPoly  -- ^ polygon to edit
        -> (Int, Point)   -- ^ index of start point of the edit, and a point to replace it with
        -> (Int, Point)   -- ^ index of end point of the edit, and a point to replace it with
        -> PointListPoly  -- ^ polygon that is produced
editPlp (PointListPoly ps) p1 p2 = PointListPoly $ editChunk ps p1 p2

------------------------------------------------------------

-- | Edits a chunk of a list.
-- Modifies the list by removing a contiguous range of elements and adding two elements in their
-- place. For example,
--   l = [ a, b, c, d, e, f, g ]
--   editChunk l (2, c') (5, f') = [  a, b, c', f',  g ]
--   editChunk l (5, f') (1, b') = [ b', c,  d,  e, f' ]
-- If the start index is greater than the end index then the operation assumes that the contiguous
-- range to remove spans the end of the list and wraps back to the start.
editChunk :: [a]       -- ^ list to edit
          -> (Int, a)  -- ^ index of the start element to remove
          -> (Int, a)  -- ^ index of the end element to remove
          -> [a]       -- ^ list that is produced
editChunk as (i1,a1) (i2,a2) = if i2 > i1 then centerEdit else wrapEdit
  where
    centerEdit = take i1 as ++ [ a1, a2 ]                      ++ drop (i2+1) as
    wrapEdit   = [ a2 ]     ++ take (i1-i2-1) (drop (i2+1) as) ++ [ a1 ]

-- | Cycles through a list by sequentially putting the head element at the end of the list,
-- listing all possible combinations.
-- For instance:
-- [a, b, c] becomes [ [a, b, c], [b, c, a], [c, a, b ] ]
caterpillar :: [a] -> [[a]]
caterpillar xs = reverse $ loop (length xs - 1) [xs]
  where
    loop :: Int -> [[a]] -> [[a]]
    loop 0 xss       = xss
    loop n xss@(h:_) = loop (n-1) (headToTail h : xss)
    loop _ []        = error "Unexpected state"

-- | Transforms a list by putting the head element at the end.
-- For example: [a, b, c, d] becomes [b, c, d, a]
headToTail :: [a] -> [a]
headToTail xs = tail xs ++ [head xs]
