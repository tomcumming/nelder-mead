module Main (main) where

import Data.Sized qualified as Sized
import Math.NelderMead
import Math.NelderMead.Point (Point (..))
import Math.NelderMead.Simplex qualified as S

main :: IO ()
main = do
  -- let errFn (Point p :: Point 2 Float) = (p Sized.!! 0) ** 2 + (p Sized.!! 1) ** 2
  let errFn (Point p :: Point 2 Float) = sin (p Sized.!! 0) + cos (p Sized.!! 1)
  -- let p :: Point 2 Float = Point $ Sized.unsafeFromList' [10, 10]
  let p :: Point 2 Float = Point $ Sized.unsafeFromList' [0, 0]
  let s = S.fromPoint errFn p 1
  mapM_ print $ take 30 $ nelderMead errFn s
