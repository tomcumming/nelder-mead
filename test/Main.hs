module Main (main) where

import Control.Monad (forM_)
import Data.List.NonEmpty qualified as NE
import Math.NelderMead
import Math.NelderMead.Point (Point (..))
import Math.NelderMead.Simplex qualified as S

type V2 = (Double, Double)

instance Point Double [Double] where
  add = zipWith (+)
  scale s = fmap (s *)

-- | Minimize sin x + cos y
sinCos :: ([Double] -> Double, S.Simplex Double [Double])
sinCos = (scoreFn, s)
  where
    scoreFn ([x, y :: Double]) = sin x + cos y
    -- p = (0, 0) :: V2
    s = S.fromPoints scoreFn $ NE.fromList [[0, 0], [1, 0], [0, 1]]

poly :: ([Double] -> Double, S.Simplex Double [Double])
poly = (scoreFn, s)
  where
    target = [4, 3, 2]
    f x ps = sum $ zipWith (\a i -> a * x ** i) ps [0 ..]
    cases = [0 .. 5]
    scoreFn ps = sum $ map (\x -> (f x target - f x ps) ** 2) cases
    s =
      S.fromPoints scoreFn $
        NE.fromList [[0, 0, 0], [1, 0, 0], [0, 1, 0], [0, 0, 1]]

main :: IO ()
main = do
  -- let (scoreFn, s) = sinCos
  let (scoreFn, s) = poly
  let res = take 100 $ nelderMead id scoreFn s
  forM_ res $ \(_, s') -> do
    putStrLn $ "Best: " <> show (S.bestScore s')
    putStrLn $ "Worst: " <> show (S.worstScore s')
    putStrLn ""
  print $ last res
