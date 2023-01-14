module Blog.Menu
    ( Menu
    , menu
    , showMenu
    , Cols
    , cols
    , selected
    , unselected
    )
where


import qualified Text.Blaze.Html5              as H

import qualified Blog.Utils.RoseTree as RT
import qualified Blog.Utils.TreeZipper as TZ


data Menu = Menu (TZ.TreeZipper FilePath)
    deriving stock Show


menu :: TZ.TreeZipper FilePath -> Menu
menu = Menu


showMenu :: Menu -> H.Html
showMenu m = H.toHtml ("test" :: String)






--PATH
data Cols
    = Selected FilePath
    | Unselected FilePath
    deriving stock (Show, Eq)

selected :: FilePath -> Cols
selected = Selected

unselected :: FilePath -> Cols
unselected = Unselected

-- husk man bør tegne opefter.
cols :: TZ.TreeZipper FilePath -> RT.RoseTree Cols
cols x = up' x parent level
    where 
        parent = TZ.up x
        level = RT.roseTree (selected (TZ.datum x))
                  (fmap (RT.leaf . unselected . TZ.datum) (TZ.children x))
        up' m p l =
            case p of 
                Nothing -> l
                Just p' -> 
                        let
                            lefts = fmap (RT.leaf . unselected . TZ.datum) (TZ.lefts m)
                            rights = fmap (RT.leaf . unselected . TZ.datum) (TZ.rights m)
                        in
                            up' p' (TZ.up p') (RT.roseTree (selected (TZ.datum p')) (lefts ++ [l] ++ rights))
