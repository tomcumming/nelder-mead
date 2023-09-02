module Math.NelderMead.Simplex
  ( Simplex,
    fromPoints,
    pointsList,
    insert,
    takeWorst,
    bestScore,
    bestPoint,
    worstScore,
    shrink,
  )
where

import Control.Monad (join)
import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Semigroup.Foldable (Foldable1)
import Math.NelderMead.Point (Point (..))
import Math.NelderMead.Point qualified as P

newtype Simplex s p = Simplex {unSimplex :: M.Map s (NE.NonEmpty p)}
  deriving (Show)

simplexNotEmptyMsg :: String
simplexNotEmptyMsg = "Internal error: Simplex must not be empty"

fromPoints ::
  (Foldable1 t, Ord s) =>
  (p -> s) ->
  t p ->
  Simplex s p
fromPoints score =
  Simplex
    . M.fromListWith (<>)
    . map (\p -> (score p, NE.singleton p))
    . toList

pointsList :: Simplex s p -> NE.NonEmpty p
pointsList =
  join
    . fromMaybe (error simplexNotEmptyMsg)
    . NE.nonEmpty
    . toList
    . unSimplex

insert :: Ord s => p -> s -> Simplex s p -> Simplex s p
insert p score (Simplex s) =
  Simplex $
    M.insertWith
      (<>)
      score
      (NE.singleton p)
      s

takeWorst :: Ord s => Simplex s p -> (s, p, Maybe (Simplex s p))
takeWorst (Simplex s) = case M.maxViewWithKey s of
  Nothing -> error simplexNotEmptyMsg
  Just ((score, ps), s') ->
    let s'' = case NE.nonEmpty (NE.tail ps) of
          Nothing -> s'
          Just ps' -> M.insert score ps' s'
        m = if M.null s'' then Nothing else Just s''
     in (score, NE.head ps, Simplex <$> m)

bestScore :: Simplex s p -> s
bestScore (Simplex s) = case M.minViewWithKey s of
  Nothing -> error simplexNotEmptyMsg
  Just ((score, _), _) -> score

bestPoint :: Simplex s p -> p
bestPoint (Simplex s) = case M.minViewWithKey s of
  Nothing -> error simplexNotEmptyMsg
  Just ((_, p), _) -> NE.head p

worstScore :: Simplex s p -> s
worstScore (Simplex s) = case M.maxViewWithKey s of
  Nothing -> error simplexNotEmptyMsg
  Just ((score, _), _) -> score

shrink ::
  forall s p.
  (Point s p, Fractional s) =>
  (p -> p) ->
  Simplex s p ->
  Simplex s p
shrink normalise (Simplex s) = Simplex $ fmap scaleTowardBest <$> s
  where
    scaleTowardBest :: p -> p
    scaleTowardBest p =
      let dir = P.add (bestPoint $ Simplex s) (P.scale (-1) p)
       in normalise $ P.add p (P.scale 0.5 dir)
