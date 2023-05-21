module Math.NelderMead
  ( nelderMead,
    ContractionSide (..),
    Step (..),
  )
where

import Data.Type.Natural (Succ)
import GHC.TypeLits (KnownNat)
import Math.NelderMead.Point (Point)
import Math.NelderMead.Point qualified as P
import Math.NelderMead.Simplex (Simplex)
import Math.NelderMead.Simplex qualified as S

data ContractionSide = Inner | Outer deriving (Show)

data Step
  = Reflect
  | Extend
  | Contract ContractionSide
  | Shrink
  deriving (Show)

nelderMead ::
  (KnownNat (Succ n), Ord a, Fractional a, Ord e) =>
  (Point (Succ n) a -> e) ->
  Simplex (Succ n) e a ->
  [(Step, Simplex (Succ n) e a)]
nelderMead errFn s
  -- Only evaluate extended if reflected is already best
  | reflectedErr < S.bestError bestPts && extendedErr < reflectedErr =
      stepWith Extend extendedErr extended
  | reflectedErr < S.worstError bestPts =
      stepWith Reflect reflectedErr reflected
  | innerErr < S.worstError bestPts = stepWith (Contract Inner) innerErr inner
  | outerErr < S.worstError bestPts = stepWith (Contract Outer) outerErr outer
  | otherwise = let s' = S.shrink s in (Shrink, s') : nelderMead errFn s'
  where
    (_, worstPt, bestPts) = S.takeWorst s

    stepWith st err pt =
      let s' = S.insert pt err bestPts
       in (st, s') : nelderMead errFn s'

    bestPtsCntr = P.centroid (S.pointsList bestPts)
    toCentroid = P.add bestPtsCntr (P.scale (-1) worstPt)

    scaleAndScore k =
      let p = P.add worstPt (P.scale k toCentroid)
       in (p, errFn p)

    (reflected, reflectedErr) = scaleAndScore 2
    (extended, extendedErr) = scaleAndScore 3
    (inner, innerErr) = scaleAndScore 0.5
    (outer, outerErr) = scaleAndScore 1.5
