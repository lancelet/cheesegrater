module Graphics.CheeseGrater.Area (
  -- * Types
    Area(..)
  ) where

-- | Allows for the area of something to be computed.
class Area a where
  -- | Computes the area.
  area :: a -> Float
