module Blog.Utils.Forest
  ( Forest,
    fromTrie,
    forest,
    toList,
  )
where

import qualified Blog.Utils.RoseTree as RT
import qualified Blog.Utils.Trie as T
import qualified Data.Map as M

data Forest a = Forest [RT.RoseTree a]
  deriving stock (Eq, Ord, Show)
  deriving stock (Functor)

forest :: [RT.RoseTree a] -> Forest a
forest = Forest

toList :: Forest a -> [RT.RoseTree a]
toList (Forest xs) = xs

fromTrie :: T.Trie a -> Forest a
fromTrie trie = Forest $ children trie
  where
    children = fmap (\(k, v) -> RT.roseTree k (children v)) . M.toAscList . T.unTrie
