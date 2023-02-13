module Blog.Table
    ( Table
    , table
    , toHtml
    , toListList
    , table
    , tab
    , row
    , col
    )
where

import Text.Blaze.Internal
import Text.Blaze.Html.Renderer.String

import Data.Tuple.Extra
import Data.Function
import Polysemy
import Polysemy.Writer
import Polysemy.Reader
import Polysemy.State
import Polysemy.Output

import Control.Monad
import qualified Blog.Utils.ListZipper as LZ
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal


data Table m a where
    Col :: String -> Table m ()
    Row :: m () -> Table m ()
    Tab :: m () -> Table m ()


makeSem ''Table


table :: (Member Table r) => [LZ.ListZipper String] -> Sem r ()
table rows = tab $ forM_ rows $ \rowData -> do
                    row $ do
                        forM_ rowData $ \colData-> do
                            col colData


toHtml :: (Member (Embed MarkupM) r, Member (State [String]) r, Member (State [[String]]) r) => Sem (Table ': r) a -> Sem r a
toHtml = interpretH (\case
        Col o -> do
            modify @[String] (++ [o])
            pureT ()
        Row m -> do
            mm <- runT m
            z <- raise $ toHtml mm
            row <- get @[String]
            modify @[[String]] (++ [row])
            put @[String] []
            pureT ()
        Tab m -> do
            mm <- runT m
            z <- raise $ toHtml mm
            rows <- get @[[String]]
            embed $ forM_ rows $ \row -> do
                        H.ul $ do
                            forM_ row $ \col -> do
                                H.li $ H.toHtml col
            pureT ()
    )

toListList :: (Member (State [String]) r, Member (State [[String]]) r) => Sem (Table ': r) a -> Sem r a
toListList = interpretH (\case
        Col o -> do
            modify @[String] (++ [o])
            pureT ()
        Row m -> do
            mm <- runT m
            z <- raise $ toListList mm
            row <- get @[String]
            modify @[[String]] (++ [row])
            put @[String] []
            pureT ()
        Tab m -> do
            mm <- runT m
            z <- raise $ toListList mm
            pureT ()
    )
