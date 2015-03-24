module Graphics.CheeseGrater.VecMath (
  -- * Types
    Vec(..)
  , Point(..)
  , Line(..)
  -- * Functions
  , vApproxEq
  , vEq
  , vAdd
  , vDot
  , vSub
  , vMul
  , vLength
  , vAngle
  , pApproxEq
  , pEq
  , pAdd
  , pSubV
  , pSubP
  , intersectLines
  , intersectLinesQuick
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Test.QuickCheck     (Arbitrary (arbitrary))

------------------------------------------------------------

-- | 2D vector.
data Vec = Vec {-# UNPACK #-} !Float -- ^ x component
               {-# UNPACK #-} !Float -- ^ y component
         deriving (Eq, Show)

-- | Approximate equality for vectors.
--   Two vectors are approximately equal if the cartesian distance between their tips is less than
--   the supplied distance.
vApproxEq :: Float -> Vec -> Vec -> Bool
vApproxEq d v1 v2 = vLength (v1 `vSub` v2) < d

-- | Approximate equality for vectors (for tests).
--   An approximate equality which is loose enough for test cases.
vEq :: Vec -> Vec -> Bool
vEq v1 v2 = vApproxEq tol v1 v2
  where
    tol :: Float
    tol = max 1E-4 (1E-4 * scale)
    
    scale :: Float
    scale = max (vLength v1) (vLength v2)

-- | Arbitrary 2D vectors.
instance Arbitrary Vec where arbitrary = Vec <$> arbitrary <*> arbitrary

-- | Adds 2D vectors.
vAdd :: Vec -> Vec -> Vec
vAdd (Vec x1 y1) (Vec x2 y2) = Vec (x1+x2) (y1+y2)

-- | Dot product of 2D vectors.
vDot :: Vec -> Vec -> Float
vDot (Vec x1 y1) (Vec x2 y2) = x1 * x2 + y1 * y2

-- | Subtracts 2D vectors.
vSub :: Vec -- ^ vector to subtract from
     -> Vec -- ^ vector to subtract
     -> Vec -- ^ result vector
vSub (Vec x1 y1) (Vec x2 y2) = Vec (x1-x2) (y1-y2)

-- | Multiplies a vector by a scalar.
vMul :: Float -> Vec -> Vec
vMul c (Vec x y) = Vec (c*x) (c*y)

-- | Computes the Euclidean length of a vector.
vLength :: Vec -> Float
vLength (Vec x y) = sqrt (x*x + y*y)

-- | Computes the angle between a pair of vectors (radians).
--   This is the clockwise angle from the first vector to the second.
vAngle :: Vec -> Vec -> Float
vAngle v1@(Vec x1 y1) v2@(Vec x2 y2) = asin (det / den)
  where
    det = x1*y2 - x2*y1
    den = vLength v1 * vLength v2

------------------------------------------------------------

-- | 2D point.
data Point = Point {-# UNPACK #-} !Float -- ^ x component
                   {-# UNPACK #-} !Float -- ^ y component
           deriving (Eq, Show)

-- | Approximate equality for points.
--   Two points are approximately equal if the cartesian distance between them is less than
--   the supplied distance.
pApproxEq :: Float -> Point -> Point -> Bool
pApproxEq d p1 p2 = vLength (p1 `pSubP` p2) < d

-- | Approximate equality for points (for tests).
--   An approximate equality which is loose enough for test cases.
pEq :: Point -> Point -> Bool
pEq (Point x1 y1) (Point x2 y2) = Vec x1 y1 `vEq` Vec x2 y2

-- | Arbitrary 2D points.
instance Arbitrary Point where arbitrary = Point <$> arbitrary <*> arbitrary

-- | Adds a vector to a point.
pAdd :: Point -> Vec -> Point
pAdd (Point px py) (Vec vx vy) = Point (px + vx) (py + vy)

-- | Subtracts a vector from a point.
pSubV :: Point -> Vec -> Point
pSubV (Point px py) (Vec vx vy) = Point (px - vx) (py - vy)

-- | Subtracts a point from another, giving the vector from the second to the first.
pSubP :: Point -- ^ tail of final vector
      -> Point -- ^ head of returned vector
      -> Vec   -- ^ returned vector; from the second point to the first point
pSubP (Point x1 y1) (Point x2 y2) = Vec (x1 - x2) (y1 - y2)

------------------------------------------------------------

-- | 2D line.
data Line = Line {-# UNPACK #-} !Point -- ^ point on the line
                 {-# UNPACK #-} !Vec   -- ^ vector along the line
          deriving (Show)

-- | Computes the intersection point between two lines.
--   If the lines are almost parallel (the angle between them is less than the first argument),
--   then the method returns `Nothing`.
intersectLines :: Float        -- ^ minimum angle between lines (radians)
               -> Line         -- ^ first line
               -> Line         -- ^ second line
               -> Maybe Point  -- ^ intersection point
intersectLines angle l1@(Line _ v1) l2@(Line _ v2) =
  if vAngle v1 v2 <= angle
     then Nothing
     else Just $ intersectLinesQuick l1 l2

-- | Computes the intersection point between two lines.
--   This method does not check whether the lines are almost parallel before proceeding; it
--   presumes that the intersection point exists.
intersectLinesQuick :: Line -> Line -> Point
intersectLinesQuick (Line o@(Point x1 y1) v@(Vec p1 q1)) (Line (Point x2 y2) (Vec p2 q2)) =
  o `pAdd` (s `vMul` v)
  where
    -- There is a danger here of causing a divide-by-zero. So, we choose between two different
    -- calculation options; either the "alpha" calculation (sa), or the "beta" calculation (sb).
    s     = if abs q2 > abs p2 then sa else sb
    sa    = (x2 - x1 + alpha * (y1 - y2)) / (p1 - alpha * q1)
    sb    = (y2 - y1 +  beta * (x1 - x2)) / (q1 -  beta * p1)
    alpha = p2 / q2
    beta  = q2 / p2
