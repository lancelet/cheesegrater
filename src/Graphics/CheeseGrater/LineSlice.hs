module Graphics.CheeseGrater.LineSlice (
  -- * Types
    LineSlice(..)
  ) where

import Graphics.CheeseGreater.VecMath (Point, Vec)

-- | Allows for something to be sliced along a line.
class LineSlice a where
  -- | Slices something along a line.
  lineSlice :: a      -- ^ thing to slice
            -> Point  -- ^ point that lies on the slicing line
            -> Vec    -- ^ vector normal to the line, pointing in the direction of the part to keep
            -> a      -- ^ returned slice
