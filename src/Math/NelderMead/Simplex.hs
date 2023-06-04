module Math.NelderMead.Simplex
  ( Simplex',
    Simplex,
    fromPoints,
    fromPoint,
    pointsList,
    insert,
    takeWorst,
    bestError,
    worstError,
    shrink,
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sized (Sized)
import Data.Sized qualified as Sized
import Data.Type.Natural (Succ)
import Data.Vector (Vector, imap)
import GHC.TypeLits (KnownNat)
import Math.NelderMead.Point (Point (..))
import Math.NelderMead.Point qualified as P

simplexNotEmptyMsg :: String
simplexNotEmptyMsg = "Unreachable: Simplex checked nonempty"

type Simplex n e a = Simplex' (Succ n) n e a

newtype Simplex' d n e a = Simplex' (M.Map e (NE.NonEmpty (Point n a)))
  deriving (Show)

fromPoints ::
  Ord e =>
  (Point n a -> e) ->
  Sized Vector (Succ n) (Point n a) ->
  Simplex n e a
fromPoints errFn =
  Simplex'
    . M.fromListWith (<>)
    . map (\p -> (errFn p, NE.singleton p))
    . toList

-- | Create a Simplex from a point by extending by offset in each dimension
fromPoint ::
  (Ord e, Num a, KnownNat n, KnownNat (Succ n)) =>
  (Point n a -> e) ->
  Point n a ->
  a ->
  Simplex n e a
fromPoint errFn (Point p) offset = fromPoints errFn (Sized.unsafeFromList' ps)
  where
    ps =
      Point p : do
        idx <- [0 .. Sized.length p - 1]
        let p' = imap (\i x -> x + if i == idx then offset else 0) $ Sized.unsized p
        [Point $ Sized.unsafeToSized' p']

pointsList :: Simplex' (Succ d) n e a -> NE.NonEmpty (Point n a)
pointsList (Simplex' s) =
  fromMaybe (error simplexNotEmptyMsg)
    . NE.nonEmpty
    $ foldMap toList s

insert :: Ord e => Point n a -> e -> Simplex' d n e a -> Simplex' (Succ d) n e a
insert p err (Simplex' s) =
  Simplex' $
    M.insertWith
      (<>)
      err
      (NE.singleton p)
      s

takeWorst :: Ord e => Simplex' (Succ d) n e a -> (e, Point n a, Simplex' d n e a)
takeWorst (Simplex' s) = case M.maxViewWithKey s of
  Nothing -> error simplexNotEmptyMsg
  Just ((e, ps), s') ->
    let s'' = case NE.nonEmpty (NE.tail ps) of
          Nothing -> s'
          Just ps' -> M.insert e ps' s'
     in (e, NE.head ps, Simplex' s'')

bestError :: Simplex' (Succ d) n e a -> e
bestError (Simplex' s) = case M.minViewWithKey s of
  Nothing -> error simplexNotEmptyMsg
  Just ((e, _), _) -> e

worstError :: Simplex' (Succ d) n e a -> e
worstError (Simplex' s) = case M.maxViewWithKey s of
  Nothing -> error simplexNotEmptyMsg
  Just ((e, _), _) -> e

shrink ::
  (KnownNat n, Ord a, Fractional a) =>
  (Point n a -> Point n a) ->
  Simplex' (Succ d) n e a ->
  Simplex' (Succ d) n e a
shrink normalise (Simplex' s) = Simplex' $ fmap scaleTowardBest <$> s
  where
    bestPoint =
      P.centroid
        . fst
        . fromMaybe (error simplexNotEmptyMsg)
        $ M.minView s

    scaleTowardBest p =
      let dir = P.add bestPoint (P.scale (-1) p)
       in normalise $ P.add p (P.scale 0.5 dir)
