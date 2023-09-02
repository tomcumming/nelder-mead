module Math.NelderMead.Point (Point (..), centroid) where

import Data.Semigroup.Foldable
import Prelude

class Point s p | p -> s where
  add :: p -> p -> p
  scale :: s -> p -> p

-- | Center of a non-empty bag of points
centroid :: (Foldable1 t, Point s p, Fractional s) => t p -> p
centroid ps = scale (1 / fromIntegral (length ps)) $ foldl1 add ps
