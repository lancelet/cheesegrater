module Graphics.CheeseGrater.VecMath
       ( Vec2(..)
       , vAdd
       , vSub
       ) where

import           Control.Applicative ((<$>), (<*>))
import           Data.AEq            (AEq (..))
import           Test.QuickCheck     (Arbitrary (arbitrary))

data Vec2 = Vec2 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
          deriving (Show, Eq)

instance AEq Vec2 where
  (===) (Vec2 x1 y1) (Vec2 x2 y2) = (x1 === x2) && (y1 === y2)
  (~==) (Vec2 x1 y1) (Vec2 x2 y2) = (x1 ~== x2) && (y1 ~== y2)

instance Arbitrary Vec2 where arbitrary = Vec2 <$> arbitrary <*> arbitrary


vAdd :: Vec2 -> Vec2 -> Vec2
vAdd (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)

vSub :: Vec2 -> Vec2 -> Vec2
vSub (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1-x2) (y1-y2)
