module Math.NelderMead.Point (Point (..), add, scale, zero, centroid) where

import Data.Foldable (foldl')
import Data.Semigroup.Foldable (Foldable1)
import Data.Sized (Sized)
import Data.Sized qualified as Sized
import Data.Vector (Vector)
import GHC.TypeLits (KnownNat)
import Prelude hiding (replicate)

newtype Point n a = Point {unPoint :: Sized Vector n a} deriving (Show, Foldable, Functor)

zero :: forall n a. (KnownNat n, Num a) => Point n a
zero = Point $ Sized.replicate' 0

add :: (KnownNat n, Num a) => Point n a -> Point n a -> Point n a
add (Point l) (Point r) = Point $ Sized.zipWith (+) l r

scale :: Num a => a -> Point n a -> Point n a
scale k = fmap (k *)

-- | Center of a non-empty bag of points
centroid :: (Foldable1 t, KnownNat n, Ord a, Fractional a) => t (Point n a) -> Point n a
centroid ps = scale (1 / fromIntegral (length ps)) $ foldl' add zero ps
