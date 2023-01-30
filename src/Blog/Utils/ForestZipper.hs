module Blog.Utils.ForestZipper
    ( ForestZipper
    , datum
    , fromForest
    , down
    , up
    , backward
    , forward
    , siblings
    , ancestors
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


toListZipper :: ForestZipper a -> LZ.ListZipper (TZ.TreeZipper a)
toListZipper (ForestZipper x) = x


siblings :: Eq a => ForestZipper a -> [TZ.TreeZipper a]
siblings fz@(ForestZipper lz) = case parent of
                Nothing -> LZ.toList lz 
                Just ok -> TZ.siblings $ LZ.focus $ toListZipper fz
    where
        parent = up fz

ancestors :: Eq a => ForestZipper a -> [[a]]
ancestors fz = fmap TZ.datum <$> ancestors' [] fz
    where
        ancestors' acc fz' = case parent of
                              Nothing -> acc
                              Just fz'' -> ancestors' (siblings fz' : acc) fz''
                    where
                        parent = up fz'

fromForest :: RT.Forest a -> Maybe (ForestZipper a)
fromForest xs = ForestZipper <$> LZ.fromList (TZ.fromRoseTree <$> RT.toList xs)


down :: (Eq a) => a -> ForestZipper a -> Maybe (ForestZipper a)
down x (ForestZipper lz) = (\y -> ForestZipper (LZ.setFocus y lz)) <$> TZ.down x (LZ.focus lz)


up :: ForestZipper a -> Maybe (ForestZipper a)
up (ForestZipper lz) = (\y -> ForestZipper (LZ.setFocus y lz)) <$> TZ.up (LZ.focus lz)


backward :: ForestZipper a -> Maybe (ForestZipper a)
backward (ForestZipper lz) = ForestZipper <$> LZ.backward lz


forward :: ForestZipper a -> Maybe (ForestZipper a)
forward (ForestZipper lz) = ForestZipper <$> LZ.forward lz
