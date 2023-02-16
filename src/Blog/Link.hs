module Blog.Link
  ( Link,
    link,
    name,
    href,
    linkFromTreeZipper,
  )
where

import qualified Blog.Utils.TreeZipper as TZ

data Link = Link
  { name :: String,
    href :: String
  }
  deriving stock (Show)
  deriving stock (Eq)

link :: String -> String -> Link
link = Link

linkFromTreeZipper :: TZ.TreeZipper FilePath -> Link
linkFromTreeZipper tz = Link (TZ.datum tz) (mconcat $ TZ.datum <$> TZ.parents tz)
