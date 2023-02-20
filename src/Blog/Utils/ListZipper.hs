module Blog.Utils.ListZipper
  ( ListZipper,
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

import Control.Comonad
import qualified Data.List.NonEmpty as NE
import Data.Maybe

data ListZipper a = ListZipper [a] a [a]
  deriving stock (Eq, Show)
  deriving stock (Functor)
  deriving stock (Foldable)
  deriving stock (Traversable)

instance Comonad ListZipper where
  extract (ListZipper _ x _) = x
  duplicate a = ListZipper (shift backward) a (shift forward)
    where
      shift move = NE.tail $ iterate move a
      iterate f x = case f x of
        Just x' -> NE.cons x (iterate f x')
        Nothing -> NE.singleton x

listZipper :: [a] -> a -> [a] -> ListZipper a
listZipper ls x rs = ListZipper ls x rs

setFocus :: a -> ListZipper a -> ListZipper a
setFocus y (ListZipper ls _ rs) = listZipper ls y rs

mapFocus :: (a -> a) -> ListZipper a -> ListZipper a
mapFocus f (ListZipper ls x rs) = listZipper ls (f x) rs

fromList :: [a] -> Maybe (ListZipper a)
fromList [] = Nothing
fromList (x : xs) = Just $ listZipper [] x xs

toList :: ListZipper a -> [a]
toList x = lefts x <> (extract x : rights x)

backward :: ListZipper a -> Maybe (ListZipper a)
backward (ListZipper (l : ls) a rs) = Just (listZipper ls l (a : rs))
backward (ListZipper [] _ _) = Nothing

forward :: ListZipper a -> Maybe (ListZipper a)
forward (ListZipper ls a (r : rs)) = Just (listZipper (a : ls) r rs)
forward (ListZipper _ _ []) = Nothing

lefts :: ListZipper a -> [a]
lefts (ListZipper ls _ _) = reverse ls

rights :: ListZipper a -> [a]
rights (ListZipper _ _ rs) = rs
