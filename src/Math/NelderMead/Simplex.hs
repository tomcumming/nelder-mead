module Math.NelderMead.Simplex
  ( Simplex',
    Simplex,
    fromPoints,
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
import Data.Type.Natural (Succ)
import Data.Vector (Vector)
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

shrink :: (KnownNat n, Ord a, Fractional a) => Simplex' (Succ d) n e a -> Simplex' (Succ d) n e a
shrink (Simplex' s) = Simplex' $ fmap scaleTowardBest <$> s
  where
    bestPoint =
      P.centroid
        . fst
        . fromMaybe (error simplexNotEmptyMsg)
        $ M.minView s

    scaleTowardBest p =
      let dir = P.add bestPoint (P.scale (-1) p)
       in P.add p (P.scale 0.5 dir)
