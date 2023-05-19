module Math.NelderMead
  ( Point (..),
    Points,
    ContractionSide (..),
    Step (..),
    mkPoints,
    bestPoint,
    debugPoints,
    nelderMead,
  )
where

import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as V

newtype Point scalar = Point (Vector scalar) deriving (Show, Eq, Ord)

newtype Points err scalar = Points (Map err (Vector (Point scalar))) deriving (Show, Eq)

data ContractionSide = Inner | Outer deriving (Show)

data Step
  = Reflect
  | Extend
  | Contract ContractionSide
  | Shrink
  deriving (Show)

pointsEmptyMsg :: String
pointsEmptyMsg = "Unreachable: Points should never be empty"

mkPoints ::
  (Foldable t, Ord err) =>
  (Point scalar -> err) ->
  t (Point scalar) ->
  Maybe (Points err scalar)
mkPoints errFn ps
  | null ps = Nothing
  | otherwise = Just $ Points $ M.fromListWith (<>) $ (\p -> (errFn p, V.singleton p)) <$> toList ps

bestPoint :: Points err scalar -> (err, Point scalar)
bestPoint (Points ps) = fromMaybe (error pointsEmptyMsg) $ do
  ((err, pss), _) <- M.minViewWithKey ps
  p <- pss V.!? 0
  pure (err, p)

debugPoints :: Points err scalar -> [Point scalar]
debugPoints (Points ps) = foldMap V.toList $ M.elems ps

addPoint :: Num scalar => Point scalar -> Point scalar -> Point scalar
addPoint (Point l) (Point r) = Point $ V.zipWith (+) l r

scalePoint :: Num scalar => scalar -> Point scalar -> Point scalar
scalePoint k (Point xs) = Point $ (k *) <$> xs

pointsSize :: Fractional scalar => Points err scalar -> scalar
pointsSize (Points ps) = sum $ realToFrac . V.length <$> ps

centroid :: Fractional scalar => Points err scalar -> Point scalar
centroid (Points ps) = scalePoint (1 / pointsSize (Points ps)) . foldl1 addPoint . foldMap toList $ M.elems ps

splitWorst :: Ord err => Points err scalar -> (Points err scalar, Point scalar)
splitWorst (Points ps) = case M.maxViewWithKey ps of
  Nothing -> error pointsEmptyMsg
  Just ((err, worst), ps')
    | p <- V.head worst,
      worst' <- V.tail worst ->
        if V.null worst'
          then (Points ps', p)
          else (Points $ M.insert err worst' ps', p)

worstErr :: Points err scalar -> err
worstErr (Points ps) =
  fst . fst
    $ fromMaybe
      (error pointsEmptyMsg)
    $ M.maxViewWithKey ps

bestErr :: Points err scalar -> err
bestErr (Points ps) =
  fst . fst
    $ fromMaybe
      (error pointsEmptyMsg)
    $ M.minViewWithKey ps

insertPoint :: Ord err => Point scalar -> err -> Points err scalar -> Points err scalar
insertPoint p e (Points ps) =
  Points $
    M.insertWith
      (<>)
      e
      (V.singleton p)
      ps

shrink :: (Fractional scalar, Ord err) => Points err scalar -> Points err scalar
shrink (Points ps) =
  Points $
    M.insert best bestPs $
      fmap scaleTowardBest <$> ps'
  where
    ((best, bestPs), ps') = fromMaybe (error pointsEmptyMsg) $ M.minViewWithKey ps
    c = centroid $ Points $ M.singleton bestErr bestPs
    scaleTowardBest p =
      let v = addPoint c (scalePoint (-1) p)
       in addPoint p (scalePoint 0.5 v)

nelderMead ::
  (Fractional scalar, Ord err) =>
  (Point scalar -> err) ->
  Points err scalar ->
  [(Step, Points err scalar)]
nelderMead errFn ps
  | errFn reflected >= worstErr bestPs =
      if
          | errFn contractedInner < worstErr bestPs
              && errFn contractedInner < errFn contractedOuter ->
              let ps' = insertPoint contractedInner (errFn contractedInner) bestPs
               in (Contract Inner, ps') : nelderMead errFn ps'
          | errFn contractedOuter < worstErr bestPs ->
              let ps' = insertPoint contractedInner (errFn contractedOuter) bestPs
               in (Contract Outer, ps') : nelderMead errFn ps'
          | otherwise ->
              let ps' = shrink ps
               in (Shrink, ps') : nelderMead errFn ps'
  | errFn reflected >= bestErr ps || errFn extended <= errFn reflected =
      let ps' = insertPoint reflected (errFn reflected) bestPs
       in (Reflect, ps') : nelderMead errFn ps'
  | otherwise =
      let ps' = insertPoint extended (errFn extended) bestPs
       in (Extend, ps') : nelderMead errFn ps'
  where
    (bestPs, worstP) = splitWorst ps
    toCentroid = addPoint (centroid bestPs) (scalePoint (-1) worstP)

    reflected = addPoint worstP (scalePoint 2 toCentroid)
    extended = addPoint worstP (scalePoint 3 toCentroid)
    contractedInner = addPoint worstP (scalePoint 0.5 toCentroid)
    contractedOuter = addPoint worstP (scalePoint 1.5 toCentroid)
