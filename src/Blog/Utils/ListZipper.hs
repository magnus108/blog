module Blog.Utils.ListZipper
    ( ListZipper
    , focus
    , setFocus
    , fromList
    , backward
    , forward
    , lefts
    , rights
    )
where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data ListZipper a = ListZipper [a] a [a]
    deriving stock (Eq,Show)



setFocus :: a -> ListZipper a -> ListZipper a
setFocus y (ListZipper ls _ rs) = ListZipper ls y rs


focus :: ListZipper a -> a
focus (ListZipper _ x _) = x


fromList :: [a] -> Maybe (ListZipper a)
fromList [] = Nothing
fromList (x:xs) = Just $ ListZipper [] x xs


backward :: ListZipper a -> Maybe (ListZipper a)
backward (ListZipper (l:ls) a rs) = Just (ListZipper ls l (a:rs))
backward (ListZipper [] _ _) = Nothing


forward :: ListZipper a -> Maybe (ListZipper a)
forward (ListZipper ls a (r:rs)) = Just (ListZipper (a:ls) r rs)
forward (ListZipper _ _ []) = Nothing


lefts :: ListZipper a -> [a]
lefts (ListZipper ls _ _) = reverse ls


rights :: ListZipper a -> [a]
rights (ListZipper _ _ rs) = rs
