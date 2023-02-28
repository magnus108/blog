module Blog.Link
  ( Link,
    link,
    name,
    href,
    fromTreeZipper,
    Row,
    row,
    unRow,
    cx,
  )
where

import qualified Blog.Utils.ListZipper as LZ
import qualified Blog.Utils.TreeZipper as TZ

data Link = Link
  { name :: String,
    href :: String
  }
  deriving stock (Show)
  deriving stock (Eq)

link :: String -> String -> Link
link = Link

fromTreeZipper :: TZ.TreeZipper FilePath -> Link
fromTreeZipper tz = Link (TZ.datum tz) ("/" ++ (mconcat (TZ.datum <$> TZ.parents tz)))

data Row a = Row
  { cx :: [String],
    unRow :: a
  }
  deriving stock (Show)
  deriving stock (Eq)

row :: [String] -> a -> Row a
row = Row
