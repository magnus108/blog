module Blog.Menu
  ( Menu,
    menu,
    down,
    showMenu,
    forward,
    showMenu',
    makeMenu,
    toForestZipper
  )
where

-------------------------------------------------------------------------------
import Text.Blaze.Internal
import Polysemy
import Data.Function
import Polysemy.State
import Data.Maybe
import Control.Monad
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


down :: FilePath -> Menu -> Maybe Menu
down x = fmap menu . FZ.down x . toForestZipper

forward :: Menu -> Maybe Menu
forward = fmap menu . FZ.forward . toForestZipper


makeMenu :: [FilePath] -> Maybe Menu
makeMenu = fmap menu . FZ.fromForest . R.fromTrie . Tr.trie . fmap splitPath


toForestZipper :: Menu -> FZ.ForestZipper FilePath
toForestZipper (Menu tz) = tz


showMenu' :: (Member T.Table r) => Menu -> Sem r ()
showMenu' m = do
    let xs = FZ.ancestors $ toForestZipper m
    let tableData = catMaybes $ fmap LZ.fromList xs
    T.table tableData


showMenu :: Menu -> H.Html
showMenu m = showMenu' m & T.toHtml
                            & evalState @[String] []
                            & evalState @[[String]] []
                            & runM
