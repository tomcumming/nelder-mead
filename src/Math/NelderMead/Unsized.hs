module Math.NelderMead.Unsized (nelderMead) where

import Data.Sized qualified as Sized
import Data.Type.Natural qualified as Nat
import Data.Vector qualified as V
import Math.NelderMead (Step)
import Math.NelderMead qualified as NM
import Math.NelderMead.Point (Point (..), unPoint)
import Math.NelderMead.Simplex (bestError, bestPoint, fromPoint)

nelderMead ::
  (Fractional a, Ord a, Ord e) =>
  (V.Vector a -> V.Vector a) ->
  (V.Vector a -> e) ->
  V.Vector a ->
  a ->
  [(Step, e, V.Vector a)]
nelderMead norm errFn initialP off = case Sized.toSomeSized initialP of
  Sized.SomeSized (Nat.Succ sn) sv -> Nat.withKnownNat (Nat.Succ sn) (nelderMead' norm errFn sv off)
  Sized.SomeSized _ _ -> error "Zero dimension initial point"

nelderMead' ::
  forall n a e.
  (Fractional a, Ord a, Ord e, Nat.KnownNat (Nat.Succ n)) =>
  (V.Vector a -> V.Vector a) ->
  (V.Vector a -> e) ->
  Sized.Sized V.Vector (Nat.Succ n) a ->
  a ->
  [(Step, e, V.Vector a)]
nelderMead' norm errFn initialP off =
  (\(s, smp) -> (s, bestError smp, Sized.unsized $ unPoint $ bestPoint smp))
    <$> NM.nelderMead @n
      (Point . Sized.unsafeToSized' . norm . Sized.unsized . unPoint)
      (errFn . Sized.unsized . unPoint)
      (fromPoint sErrFn (Point initialP) off)
  where
    sErrFn = errFn . Sized.unsized . unPoint
