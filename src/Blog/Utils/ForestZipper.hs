module Blog.Utils.ForestZipper
  ( ForestZipper,
    fromForest,
    datum,
    down,
    downTo,
    up,
    backward,
    forward,
    siblings,
    moveTo,
    ancestors,
  )
where

import qualified Blog.Utils.Forest as F
import qualified Blog.Utils.ListZipper as LZ
import qualified Blog.Utils.TreeZipper as TZ
import Control.Comonad

data ForestZipper a = ForestZipper (LZ.ListZipper (TZ.TreeZipper a))
  deriving stock (Show)
  deriving stock (Eq)

datum :: ForestZipper a -> a
datum (ForestZipper lz) = TZ.datum $ extract lz

toListZipper :: ForestZipper a -> LZ.ListZipper (TZ.TreeZipper a)
toListZipper (ForestZipper x) = x

siblings :: Eq a => ForestZipper a -> [TZ.TreeZipper a]
siblings fz@(ForestZipper lz) = case parent of
  Nothing -> LZ.toList lz
  Just ok -> TZ.siblings $ extract $ toListZipper fz
  where
    parent = up fz

ancestors :: Eq a => ForestZipper a -> [[TZ.TreeZipper a]]
ancestors fz = ancestors' [] fz
  where
    ancestors' acc fz' = case parent of
      Nothing -> siblings fz' : acc
      Just fz'' -> ancestors' (siblings fz' : acc) fz''
      where
        parent = up fz'

fromForest :: F.Forest a -> Maybe (ForestZipper a)
fromForest xs = ForestZipper <$> LZ.fromList (TZ.fromRoseTree <$> F.toList xs)

down :: (Eq a) => a -> ForestZipper a -> Maybe (ForestZipper a)
down xs (ForestZipper lz) =
  ForestZipper . flip LZ.setFocus lz
    <$> TZ.down xs (extract lz)

downTo :: (Eq a) => [a] -> ForestZipper a -> Maybe (ForestZipper a)
downTo xs (ForestZipper lz) =
  ForestZipper . flip LZ.setFocus lz
    <$> TZ.downTo xs (extract lz)

moveTo :: Eq a => [a] -> ForestZipper a -> Maybe (ForestZipper a)
moveTo [] tz = Just tz
moveTo (x : xs) tz = downTo xs =<< findIt x tz
  where
    findIt x' tz' =
      if (x' == (datum tz'))
        then Just tz'
        else case forward tz' of
          Nothing -> Nothing
          Just tz'' -> findIt x' tz''

up :: ForestZipper a -> Maybe (ForestZipper a)
up (ForestZipper lz) =
  ForestZipper . flip LZ.setFocus lz
    <$> TZ.up (extract lz)

setFocus :: ForestZipper a -> TZ.TreeZipper a -> ForestZipper a
setFocus (ForestZipper lz) a = ForestZipper $ LZ.setFocus a lz

backward :: ForestZipper a -> Maybe (ForestZipper a)
backward (ForestZipper lz) = ForestZipper <$> LZ.backward lz

forward :: ForestZipper a -> Maybe (ForestZipper a)
forward (ForestZipper lz) = ForestZipper <$> LZ.forward lz
