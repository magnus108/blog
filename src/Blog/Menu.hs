module Blog.Menu
  ( Menu,
    menu,
    makeMenu,
    showMenu,
  )
where

import qualified Blog.Link as Link
import qualified Blog.Table as T
import qualified Blog.Utils.Forest as F
import qualified Blog.Utils.ForestZipper as FZ
import qualified Blog.Utils.ListZipper as LZ
import qualified Blog.Utils.Trie as Tr
import Data.Maybe
import Polysemy
import System.FilePath (splitPath)

newtype Menu a = Menu (a -> Maybe (FZ.ForestZipper FilePath))

menu :: (a -> Maybe (FZ.ForestZipper FilePath)) -> Menu a
menu = Menu

makeMenu :: [FilePath] -> Menu FilePath
makeMenu xs = menu (\x -> FZ.moveTo (splitPath x) =<< fz)
  where
    fz = FZ.fromForest $ F.fromTrie $ Tr.trie $ fmap splitPath xs

showMenu :: (Member T.Table r) => FilePath -> Menu FilePath -> Sem r ()
showMenu x (Menu f) = do
  let fz = f x
  let xs = FZ.ancestors <$> fz
  let tableData = mapMaybe LZ.fromList <$> xs
  T.table (fmap Link.fromTreeZipper <$> (fromMaybe [] tableData))
