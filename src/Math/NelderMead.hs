module Math.NelderMead
  ( step,
    nelderMead,
    ContractionSide (..),
    Step (..),
  )
where

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
  (Point s p, Fractional s, Ord s) =>
  (p -> p) ->
  (p -> s) ->
  Simplex s p ->
  [(Step, Simplex s p)]
nelderMead norm score s =
  let (st, s') = step norm score s
   in (st, s') : nelderMead norm score s'

step ::
  (Point s p, Fractional s, Ord s) =>
  (p -> p) ->
  (p -> s) ->
  Simplex s p ->
  (Step, Simplex s p)
step normalise scoreFn s
  -- Only evaluate extended if reflected is already best
  | reflectedScore < S.bestScore bestPts && extendedScore < reflectedScore =
      stepWith Extend extendedScore extended
  | reflectedScore < S.worstScore bestPts =
      stepWith Reflect reflectedScore reflected
  | innerScore < S.worstScore bestPts =
      stepWith (Contract Inner) innerScore inner
  | outerScore < S.worstScore bestPts =
      stepWith (Contract Outer) outerScore outer
  | otherwise = (Shrink, S.shrink normalise s)
  where
    (worstPt, bestPts) = case S.takeWorst s of
      (_, worstPt', Just bestPts') -> (worstPt', bestPts')
      _ -> error "You need at least two points to run nelderMead"

    stepWith st score pt = (st, S.insert pt score bestPts)

    bestPtsCntr = P.centroid (S.pointsList bestPts)
    toCentroid = P.add bestPtsCntr (P.scale (-1) worstPt)

    scaleAndScore k =
      let p = normalise $ P.add worstPt (P.scale k toCentroid)
       in (p, scoreFn p)

    (reflected, reflectedScore) = scaleAndScore 2
    (extended, extendedScore) = scaleAndScore 3
    (inner, innerScore) = scaleAndScore 0.5
    (outer, outerScore) = scaleAndScore 1.5
