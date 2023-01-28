module Blog.Menu
  ( Menu,
    menu,
    showMenu,
    makeMenu,
    toForestZipper
  )
where

-------------------------------------------------------------------------------

import qualified Blog.Table as T
import qualified Blog.Utils.ForestZipper as FZ
import qualified Blog.Utils.ListZipper as LZ
import qualified Blog.Utils.RoseTree as R
import qualified Blog.Utils.RoseTree as RT
import qualified Blog.Utils.Trie as Tr
import System.FilePath (splitPath)
import qualified Text.Blaze.Html5 as H

-------------------------------------------------------------------------------

data Menu = Menu (FZ.ForestZipper FilePath)
  deriving stock (Show)

menu :: FZ.ForestZipper FilePath -> Menu
menu = Menu

makeMenu :: [FilePath] -> Maybe Menu
makeMenu = fmap menu . FZ.fromForest . R.fromTrie . Tr.trie . fmap splitPath

toForestZipper :: Menu -> FZ.ForestZipper FilePath
toForestZipper (Menu tz) = tz

showMenu :: Menu -> H.Html
showMenu m = do
  -- let fz = toForestZipper m
  -- let table = foldUp (\x -> _) [] fz
  let tableData = [LZ.listZipper [] "a" []]
  T.runTableHTML $ T.mkTable tableData
--  T.runTableHTML $ forM_ tableData 
 --                           (\rowData -> col $ forM_ rowData row)
