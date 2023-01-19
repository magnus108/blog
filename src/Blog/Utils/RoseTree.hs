module Blog.Utils.RoseTree
    ( RoseTree
    , Forest
    , roseTree
    , datum
    , children
    , leaf
    , fromTrie
    , forest
    , toList
    )
where
--------------------------------------------------------------------------------
import qualified Data.Map                      as M
import qualified Data.Maybe                    as Maybe

--------------------------------------------------------------------------------
import qualified Blog.Utils.Trie as T

--------------------------------------------------------------------------------

data RoseTree a = RoseTree a [RoseTree a]
    deriving (Eq, Ord, Show)
    deriving (Functor)


data Forest a = Forest [RoseTree a]
    deriving (Eq, Ord, Show)
    deriving (Functor)


forest :: [RoseTree a] -> Forest a
forest = Forest


toList :: Forest a -> [RoseTree a]
toList (Forest xs) = xs


roseTree :: a -> [RoseTree a] -> RoseTree a
roseTree = RoseTree


leaf :: a -> RoseTree a
leaf x = RoseTree x []

datum :: RoseTree a -> a
datum (RoseTree x _) = x


children :: RoseTree a -> [RoseTree a]
children (RoseTree _ xs) = xs


fromTrie :: T.Trie a -> Forest a
fromTrie trie = Forest $ children trie
    where
        children = fmap (\(k, v) -> RoseTree k (children v)) . M.toList . T.unTrie
