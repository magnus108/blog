module Blog.Utils.RoseTree
    ( RoseTree
    , roseTree
    , datum
    , children
    , leaf
    , fromTrie
    )
where
--------------------------------------------------------------------------------
import qualified Data.Map                      as M

--------------------------------------------------------------------------------
import qualified Blog.Utils.Trie as T

--------------------------------------------------------------------------------

data RoseTree a = RoseTree a [RoseTree a]
    deriving (Eq, Ord, Show)
    deriving (Functor)


roseTree :: a -> [RoseTree a] -> RoseTree a
roseTree = RoseTree


leaf :: a -> RoseTree a
leaf x = RoseTree x []

datum :: RoseTree a -> a
datum (RoseTree x _) = x


children :: RoseTree a -> [RoseTree a]
children (RoseTree _ xs) = xs


fromTrie :: T.Trie a -> Maybe (RoseTree a)
fromTrie = undefined -- M.fmap (\(k, v) -> RoseTree k (fromTrie v))
