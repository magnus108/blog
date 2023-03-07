module Blog.Utils.RoseTree
  ( RoseTree,
    roseTree,
    datum,
    children,
    leaf,
  )
where

import Control.Comonad

data RoseTree a = RoseTree a [RoseTree a]
  deriving (Eq, Ord, Show)
  deriving (Functor)

instance Comonad RoseTree where
  duplicate w@(RoseTree _ as) = RoseTree w (map duplicate as)
  extract (RoseTree a _) = a

roseTree :: a -> [RoseTree a] -> RoseTree a
roseTree = RoseTree

leaf :: a -> RoseTree a
leaf x = RoseTree x []

datum :: RoseTree a -> a
datum (RoseTree x _) = x

children :: RoseTree a -> [RoseTree a]
children (RoseTree _ xs) = xs
