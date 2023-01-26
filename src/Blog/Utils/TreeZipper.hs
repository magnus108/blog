module Blog.Utils.TreeZipper
    ( Context
    , TreeZipper
    , fromRoseTree
    , toRoseTree
    , datum
    , down
    , up
    , firstChild
    , lefts
    , rights
    , children
    , siblings
    )
where


--------------------------------------------------------------------------------
import qualified Blog.Utils.RoseTree as RT

--------------------------------------------------------------------------------

data Context a = Context [RT.RoseTree a] a [RT.RoseTree a]
    deriving stock (Show, Eq, Ord)


data TreeZipper a = TreeZipper (RT.RoseTree a) [Context a]
    deriving stock (Show, Eq, Ord)


fromRoseTree :: RT.RoseTree a -> TreeZipper a
fromRoseTree x = TreeZipper x []


toRoseTree :: TreeZipper a -> RT.RoseTree a
toRoseTree (TreeZipper item _ ) = item


datum :: TreeZipper a -> a
datum tz = RT.datum (toRoseTree tz)


down :: Eq a => a -> TreeZipper a -> Maybe (TreeZipper a)
down x (TreeZipper rt bs) =
    let (ls, rs) = break (\item -> RT.datum item == x) (RT.children rt)
    in  case rs of
            y : ys ->
                Just (TreeZipper y (Context ls (RT.datum rt) ys : bs))
            _ -> Nothing


up :: TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper item []) = Nothing
up (TreeZipper item ((Context ls x rs) : bs)) =
    Just (TreeZipper (RT.roseTree x (ls <> [item] <> rs)) bs)


rights' :: TreeZipper a -> [RT.RoseTree a]
rights' (TreeZipper _    []                       ) = []
rights' (TreeZipper item ((Context ls x rs) : bs)) = rs

lefts' :: TreeZipper a -> [RT.RoseTree a]
lefts' (TreeZipper _    []                       ) = []
lefts' (TreeZipper item ((Context ls x rs) : bs)) = reverse ls


firstChild :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
firstChild tz = firstChild' (toRoseTree tz)
  where
    firstChild' x = case RT.children x of
        []     -> Nothing
        c : cs -> down (RT.datum c) tz


nextSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
nextSibling tz = case rights' tz of
    []          -> Nothing
    next : rest -> down (RT.datum next) =<< up tz


previousSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
previousSibling tz = case lefts' tz of
    []          -> Nothing
    prev : rest -> down (RT.datum prev) =<< up tz


rights :: Eq a => TreeZipper a -> [TreeZipper a]
rights tz =
    let next = nextSibling tz
    in  case next of
            Nothing    -> []
            Just next' -> next' : (rights next')


lefts :: Eq a => TreeZipper a -> [TreeZipper a]
lefts tz =
    let prev = previousSibling tz
    in  case prev of
            Nothing    -> []
            Just prev' -> lefts prev' ++ [prev']


children :: Eq a => TreeZipper a -> [TreeZipper a]
children tz =
    let child = firstChild tz
    in  case child of
            Nothing     -> []
            Just child' -> child' : (rights child')


siblings :: Eq a => TreeZipper a -> [TreeZipper a]
siblings tz = lefts tz ++ (tz : rights tz)
