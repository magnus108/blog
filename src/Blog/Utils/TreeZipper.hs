module Blog.Utils.TreeZipper
  ( Context,
    Context' (..),
    TreeZipper,
    context,
    fromRoseTree,
    toRoseTree,
    datum,
    parents,
    down,
    downTo,
    up,
    firstChild,
    lefts,
    rights,
    children,
    siblings,
  )
where

import qualified Blog.Utils.RoseTree as RT
import Control.Comonad
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)

data Context' w a = Context' [w a] a [w a]
  deriving stock (Eq)
  deriving stock (Show)
  deriving stock (Ord)
  deriving stock (Functor)
  deriving stock (Foldable)
  deriving stock (Traversable)

instance (Comonad w) => Comonad (Context' w) where
  extract (Context' _ a _) = a
  duplicate l@(Context' ls a rs) =
    Context'
      (shift backward'')
      l
      (shift forward'')
    where
      lol = (\x -> extend (const l) x) <$> listToMaybe (ls ++ rs)
      shift move = case lol of
        Nothing -> []
        Just x -> NE.tail $ iterate move x
      iterate f x = case f x of
        Just x' -> NE.cons x (iterate f x')
        Nothing -> NE.singleton x

context :: [w a] -> a -> [w a] -> Context' w a
context ls x rs = Context' ls x rs

backward'' :: Comonad w => w (Context' w a) -> Maybe (w (Context' w a))
backward'' x = case extract x of
  (Context' (l : ls) a rs) -> Just (extend (\l' -> context ls (extract l') (extend (const a) l' : rs)) l)
  (Context' [] _ _) -> Nothing

forward'' :: Comonad w => w (Context' w a) -> Maybe (w (Context' w a))
forward'' x = case extract x of
  (Context' ls a (r : rs)) -> Just (extend (\r' -> context (extend (const a) r' : ls) (extract r') rs) r)
  (Context' _ _ []) -> Nothing

type Context a = Context' RT.RoseTree a

data TreeZipper a = TreeZipper (RT.RoseTree a) [Context a]
  deriving stock (Show)
  deriving stock (Eq)
  deriving stock (Ord)

fromRoseTree :: RT.RoseTree a -> TreeZipper a
fromRoseTree x = TreeZipper x []

toRoseTree :: TreeZipper a -> RT.RoseTree a
toRoseTree (TreeZipper item _) = item

datum :: TreeZipper a -> a
datum tz = RT.datum (toRoseTree tz)

down :: Eq a => a -> TreeZipper a -> Maybe (TreeZipper a)
down x (TreeZipper rt bs) =
  let (ls, rs) = break (\item -> RT.datum item == x) (RT.children rt)
   in case rs of
        y : ys ->
          Just (TreeZipper y (Context' ls (RT.datum rt) ys : bs))
        _ -> Nothing

up :: TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper item []) = Nothing
up (TreeZipper item ((Context' ls x rs) : bs)) =
  Just (TreeZipper (RT.roseTree x (ls <> [item] <> rs)) bs)

parents :: TreeZipper a -> [TreeZipper a]
parents tz = parent ++ [tz]
  where
    parent = case up tz of
      Nothing -> []
      Just x -> parents x

rights' :: TreeZipper a -> [RT.RoseTree a]
rights' (TreeZipper _ []) = []
rights' (TreeZipper item ((Context' ls x rs) : bs)) = rs

lefts' :: TreeZipper a -> [RT.RoseTree a]
lefts' (TreeZipper _ []) = []
lefts' (TreeZipper item ((Context' ls x rs) : bs)) = reverse ls

firstChild :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
firstChild tz = firstChild' (toRoseTree tz)
  where
    firstChild' x = case RT.children x of
      [] -> Nothing
      c : cs -> down (RT.datum c) tz

nextSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
nextSibling tz = case rights' tz of
  [] -> Nothing
  next : rest -> down (RT.datum next) =<< up tz

previousSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
previousSibling tz = case lefts' tz of
  [] -> Nothing
  prev : rest -> down (RT.datum prev) =<< up tz

rights :: Eq a => TreeZipper a -> [TreeZipper a]
rights tz =
  let next = nextSibling tz
   in case next of
        Nothing -> []
        Just next' -> next' : rights next'

lefts :: Eq a => TreeZipper a -> [TreeZipper a]
lefts tz =
  let prev = previousSibling tz
   in case prev of
        Nothing -> []
        Just prev' -> lefts prev' ++ [prev']

children :: Eq a => TreeZipper a -> [TreeZipper a]
children tz =
  let child = firstChild tz
   in case child of
        Nothing -> []
        Just child' -> child' : rights child'

siblings :: Eq a => TreeZipper a -> [TreeZipper a]
siblings tz = lefts tz ++ (tz : rights tz)

downTo :: Eq a => [a] -> TreeZipper a -> Maybe (TreeZipper a)
downTo [] tz = Just tz
downTo (x : xs) tz = downTo xs =<< down x tz
