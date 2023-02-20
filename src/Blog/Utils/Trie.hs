module Blog.Utils.Trie
  ( Trie,
    empty,
    trie,
    insert,
    unTrie,
  )
where

import qualified Data.Map as M

data Trie a = Trie (M.Map a (Trie a))
  deriving stock (Show, Eq)

instance (Ord a) => Semigroup (Trie a) where
  (Trie m1) <> (Trie m2) = Trie (M.unionWith (<>) m1 m2)

unTrie :: Trie a -> M.Map a (Trie a)
unTrie (Trie m) = m

empty :: Trie a
empty = Trie M.empty

trie :: Ord a => [[a]] -> Trie a
trie = foldl insert empty

insert :: Ord a => Trie a -> [a] -> Trie a
insert t [] = t
insert (Trie m) (x : xs) = Trie $ M.unionWith (<>) (M.singleton x (insert empty xs)) m
