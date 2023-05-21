module Main (main) where

import Data.Sized qualified as Sized
import Math.NelderMead
import Math.NelderMead.Point (Point (..))
import Math.NelderMead.Simplex qualified as S

main :: IO ()
main = do
  let errFn (Point p :: Point 2 Float) = sin (p Sized.!! 0) + cos (p Sized.!! 1)
  -- let errFn (Point p :: Point 2 Float) = (p Sized.!! 0) ** 2 + (p Sized.!! 1) ** 2

  let ps :: [Point 2 Float] =
        Point . Sized.unsafeFromList'
          <$> [[10, 10], [10, 11], [11, 10]]
          -- <$> [[0, 0], [0, -pi / 8], [-pi / 8, 0]]

  let s :: S.Simplex 2 Float Float = S.fromPoints errFn $ Sized.unsafeFromList' ps
  mapM_ print $ take 30 $ nelderMead errFn s
