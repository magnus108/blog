module Blog.Utils.ForestZipper
    ( ForestZipper
    , datum
    , fromForest
    , down
    , up
    )
where

--------------------------------------------------------------------------------
import qualified Blog.Utils.TreeZipper as TZ
import qualified Blog.Utils.ListZipper as LZ
import qualified Blog.Utils.RoseTree   as RT

--------------------------------------------------------------------------------

data ForestZipper a = ForestZipper (LZ.ListZipper (TZ.TreeZipper a))
    deriving stock (Show, Eq)


datum :: ForestZipper a -> a
datum (ForestZipper lz)= TZ.datum $ LZ.focus lz


fromForest :: RT.Forest a -> Maybe (ForestZipper a)
fromForest xs = ForestZipper <$> LZ.fromList (TZ.fromRoseTree <$> RT.toList xs)


down :: (Eq a, Show a) => a -> ForestZipper a -> Maybe (ForestZipper a)
down x (ForestZipper lz) = (\y -> ForestZipper (LZ.setFocus y lz)) <$> TZ.down x (LZ.focus lz)


up :: ForestZipper a -> Maybe (ForestZipper a)
up (ForestZipper lz) = (\y -> ForestZipper (LZ.setFocus y lz)) <$> TZ.up (LZ.focus lz)
