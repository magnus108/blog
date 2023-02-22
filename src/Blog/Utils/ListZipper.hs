module Blog.Utils.ListZipper
  ( ListZipper,
    listZipper,
    fromList,
    toList,
    backward,
    forward,
    lefts,
    rights,
    singleton,
    setFocus,
    focus,
  )
where

import Control.Comonad
import Data.Functor.Identity
import qualified Data.List.NonEmpty as NE

data ListZipper' w a = ListZipper' [a] (w a) [a]
  deriving stock (Eq)
  deriving stock (Show)
  deriving stock (Functor)
  deriving stock (Foldable)
  deriving stock (Traversable)

type ListZipper a = ListZipper' Identity a

instance (Monad w, Comonad w) => Comonad (ListZipper' w) where
  extract (ListZipper' _ a _) = extract a
  duplicate l@(ListZipper' ls a rs) =
    ListZipper'
      (shift (backward' extract return))
      (extend (\x -> ListZipper' ls x rs) a)
      (shift (forward' extract return))
    where
      shift move = NE.tail $ iterate move l
      iterate f x = case f x of
        Just x' -> NE.cons x (iterate f x')
        Nothing -> NE.singleton x

setFocus' :: w a -> ListZipper' w a -> ListZipper' w a
setFocus' y (ListZipper' ls _ rs) = listZipper ls y rs

setFocus :: a -> ListZipper a -> ListZipper a
setFocus y lz = setFocus' (Identity y) lz

singleton :: w a -> ListZipper' w a
singleton x = listZipper [] x []

listZipper :: [a] -> w a -> [a] -> ListZipper' w a
listZipper ls x rs = ListZipper' ls x rs

fromList' :: (w a -> a) -> [w a] -> Maybe (ListZipper' w a)
fromList' f [] = Nothing
fromList' f (x : xs) = Just $ listZipper [] x (fmap f xs)

fromList :: [a] -> Maybe (ListZipper a)
fromList = fromList' runIdentity . fmap Identity

focus :: ListZipper' w a -> w a
focus (ListZipper' _ a _) = a

toList' :: (w a -> a) -> ListZipper' w a -> [a]
toList' f x = lefts x <> (f (focus x) : rights x)

toList :: ListZipper a -> [a]
toList = toList' runIdentity

backward' :: (w a -> a) -> (a -> w a) -> ListZipper' w a -> Maybe (ListZipper' w a)
backward' d u (ListZipper' (l : ls) a rs) = Just (listZipper ls (u l) (d a : rs))
backward' d u (ListZipper' [] _ _) = Nothing

backward :: ListZipper a -> Maybe (ListZipper a)
backward = backward' runIdentity Identity

forward' :: (w a -> a) -> (a -> w a) -> ListZipper' w a -> Maybe (ListZipper' w a)
forward' d u (ListZipper' ls a (r : rs)) = Just (listZipper (d a : ls) (u r) rs)
forward' d u (ListZipper' _ _ []) = Nothing

forward :: ListZipper a -> Maybe (ListZipper a)
forward = forward' runIdentity Identity

lefts :: ListZipper' w a -> [a]
lefts (ListZipper' ls _ _) = reverse ls

rights :: ListZipper' w a -> [a]
rights (ListZipper' _ _ rs) = rs
