module Blog.Menu
    ( Menu
    , menu
    , showMenu
    )
where


import qualified Text.Blaze.Html5              as H

-------------------------------------------------------------------------------
import qualified Blog.Utils.RoseTree as RT
import qualified Blog.Utils.ForestZipper as FZ
import qualified Blog.Utils.ListZipper as LZ 
import qualified Blog.Table as T

-------------------------------------------------------------------------------

data Menu = Menu (FZ.ForestZipper FilePath)
    deriving stock Show


menu :: FZ.ForestZipper FilePath -> Menu
menu = Menu


toForestZipper :: Menu -> FZ.ForestZipper FilePath
toForestZipper (Menu tz) = tz


showMenu :: Menu -> H.Html
showMenu m = do
    --let fz = toForestZipper m
    --let table = foldUp (\x -> _) [] fz
    let table = [LZ.listZipper [] "a" []]
    T.runTableHTML $ T.mkTable table
