module Graphics.CheeseGrater.ApproxEq (
  -- * Classes
    ApproxEq(..)
  -- * Functions
  , defaultRelativeTolerance
  ) where

-- | Default relative tolerance to use for floats.
defaultRelativeTolerance :: Float
defaultRelativeTolerance = 1E-4

-- | Set precedence of the ~== function.
infix 4 ~==

-- | Test for approximate equality.
-- This is really intended for testing purposes, to make comparing floating point values and
-- points, vectors, colors, etc., nice and easy. Ultimately, in tests, the (~==) method will
-- most commonly be used.
-- For a minimal definition, define 'approxEq' and 'absoluteTolFromRelativeTol'.
class ApproxEq a where

  -- | Tests for approximate equality given an absolute tolerance parameter.
  approxEq :: Float -- ^ absolute tolerance
           -> a     -- ^ first value to compare
           -> a     -- ^ second value to compare
           -> Bool  -- ^ true if the values are approximately equal; false otherwise

  -- | Computes an absolute tolerance from a relative tolerance.
  absoluteTolFromRelativeTol :: Float -- ^ relative tolerance
                             -> a     -- ^ first value participating in a comparison
                             -> a     -- ^ second value participating in a comparison
                             -> Float -- ^ absolute tolerance

  -- | Test for approximate equality.
  -- By default, this method computes an absolute tolerance to use (using the default relative
  -- tolerance as a starting point), and then performs the comparison.
  (~==) :: a      -- ^ first value to compare
        -> a      -- ^ second value to compare
        -> Bool   -- ^ true if the two values are approximately equal
  (~==) x y = approxEq absTol x y
    where
      absTol :: Float
      absTol = absoluteTolFromRelativeTol defaultRelativeTolerance x y

instance ApproxEq Float where
  approxEq t x y = abs (x - y) <= t
  absoluteTolFromRelativeTol rel x y = min rel (max (rel*abs x) (rel*abs y))
