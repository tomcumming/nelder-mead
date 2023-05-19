module Main (main) where

import Data.Vector qualified as V
import Math.NelderMead

main :: IO ()
main = do
  let errFn (Point p) = sin (p V.! 0) + cos (p V.! 1)
  -- let errFn (Point p) = (p V.! 0) ** 2 + (p V.! 1) ** 2
  let Just ps =
        mkPoints errFn $
          Point . V.fromList
            <$>
            -- [[10 :: Double, 10], [10, 11], [11, 10]]
            [[0 :: Double, 0], [0, -pi / 8], [-pi / 8, 0]]
  mapM_ print $ take 20 $ nelderMead errFn ps
