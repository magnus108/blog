module Blog.Utils.ListZipper
  ( ListZipper,
    focus,
    listZipper,
    setFocus,
    mapFocus,
    fromList,
    toList,
    backward,
    forward,
    lefts,
    rights,
  )
where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data ListZipper a = ListZipper [a] a [a]
  deriving stock (Eq, Show)
  deriving (Functor)
  deriving (Foldable)
  deriving (Traversable)

listZipper :: [a] -> a -> [a] -> ListZipper a
listZipper ls x rs = ListZipper (reverse ls) x rs

setFocus :: a -> ListZipper a -> ListZipper a
setFocus y (ListZipper ls _ rs) = ListZipper ls y rs

mapFocus :: (a -> a) -> ListZipper a -> ListZipper a
mapFocus f (ListZipper ls x rs) = ListZipper ls (f x) rs

focus :: ListZipper a -> a
focus (ListZipper _ x _) = x

fromList :: [a] -> Maybe (ListZipper a)
fromList [] = Nothing
fromList (x : xs) = Just $ ListZipper [] x xs

toList :: ListZipper a -> [a]
toList x = lefts x <> (focus x : rights x)

backward :: ListZipper a -> Maybe (ListZipper a)
backward (ListZipper (l : ls) a rs) = Just (ListZipper ls l (a : rs))
backward (ListZipper [] _ _) = Nothing

forward :: ListZipper a -> Maybe (ListZipper a)
forward (ListZipper ls a (r : rs)) = Just (ListZipper (a : ls) r rs)
forward (ListZipper _ _ []) = Nothing

lefts :: ListZipper a -> [a]
lefts (ListZipper ls _ _) = reverse ls

rights :: ListZipper a -> [a]
rights (ListZipper _ _ rs) = rs
