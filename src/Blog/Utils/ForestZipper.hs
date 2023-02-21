module Blog.Utils.ForestZipper
  ( ForestZipper,
    fromForest,
    datum,
    up,
    down,
    downTo,
    backward,
    forward,
    setFocus,
    siblings,
    ancestors,
    moveTo,
  )
where

import qualified Blog.Utils.Forest as F
import qualified Blog.Utils.ListZipper as LZ
import qualified Blog.Utils.TreeZipper as TZ
import Control.Comonad

data ForestZipper a = ForestZipper (LZ.ListZipper (TZ.TreeZipper a))
  deriving stock (Show)
  deriving stock (Eq)

toListZipper :: ForestZipper a -> LZ.ListZipper (TZ.TreeZipper a)
toListZipper (ForestZipper x) = x

fromForest :: F.Forest a -> Maybe (ForestZipper a)
fromForest xs = ForestZipper <$> LZ.fromList (TZ.fromRoseTree <$> F.toList xs)

datum :: ForestZipper a -> a
datum (ForestZipper lz) = TZ.datum $ extract lz

up :: ForestZipper a -> Maybe (ForestZipper a)
up (ForestZipper lz) = flip setFocus (ForestZipper lz) <$> TZ.up (extract lz)

down :: (Eq a) => a -> ForestZipper a -> Maybe (ForestZipper a)
down xs (ForestZipper lz) =
  flip setFocus (ForestZipper lz)
    <$> TZ.down xs (extract lz)

downTo :: (Eq a) => [a] -> ForestZipper a -> Maybe (ForestZipper a)
downTo xs (ForestZipper lz) =
  flip setFocus (ForestZipper lz)
    <$> TZ.downTo xs (extract lz)

setFocus :: TZ.TreeZipper a -> ForestZipper a -> ForestZipper a
setFocus a (ForestZipper lz) = ForestZipper $ LZ.setFocus a lz

backward :: ForestZipper a -> Maybe (ForestZipper a)
backward (ForestZipper lz) = ForestZipper <$> LZ.backward lz

forward :: ForestZipper a -> Maybe (ForestZipper a)
forward (ForestZipper lz) = ForestZipper <$> LZ.forward lz

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
