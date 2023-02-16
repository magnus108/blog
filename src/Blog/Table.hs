module Blog.Table
  ( Table,
    table,
    toHtml,
    toListList,
    tab,
    row,
    col,
    link,
  )
where

import qualified Blog.Link as Link
import qualified Blog.Utils.ListZipper as LZ
import qualified Blog.Utils.TreeZipper as TZ
import Control.Monad
import Polysemy
import Polysemy.State
import Text.Blaze (stringValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal

data Table m a where
  Link :: Link.Link -> Table m ()
  Col :: m () -> Table m ()
  Row :: m () -> Table m ()
  Tab :: m () -> Table m ()

makeSem ''Table

table :: (Member Table r) => [LZ.ListZipper (Link.Link)] -> Sem r ()
table rows = tab $ forM_ rows $ \rowData -> do
  row $ do
    forM_ rowData $ col . link

toHtml :: (Member (Embed MarkupM) r, Member (State [Link.Link]) r, Member (State [[Link.Link]]) r) => Sem (Table ': r) a -> Sem r a
toHtml =
  interpretH
    ( \case
        Link l -> do
          modify @[Link.Link] (++ [l])
          pureT ()
        Col m -> do
          mm <- runT m
          z <- raise $ toHtml mm
          pureT ()
        Row m -> do
          mm <- runT m
          z <- raise $ toHtml mm
          row <- get @[Link.Link]
          modify @[[Link.Link]] (++ [row])
          put @[Link.Link] []
          pureT ()
        Tab m -> do
          mm <- runT m
          z <- raise $ toHtml mm
          rows <- get @[[Link.Link]]
          embed $ forM_ rows $ \row -> do
            H.ul $ do
              forM_ row $ \l -> do
                H.li $ H.a ! A.href (stringValue (Link.href l)) $ H.toHtml (Link.name l)
          pureT ()
    )

toListList :: (Member (State [Link.Link]) r, Member (State [[Link.Link]]) r) => Sem (Table ': r) a -> Sem r a
toListList =
  interpretH
    ( \case
        Link l -> do
          modify @[Link.Link] (++ [l])
          pureT ()
        Col m -> do
          mm <- runT m
          z <- raise $ toListList mm
          pureT ()
        Row m -> do
          mm <- runT m
          z <- raise $ toListList mm
          row <- get @[Link.Link]
          modify @[[Link.Link]] (++ [row])
          put @[Link.Link] []
          pureT ()
        Tab m -> do
          mm <- runT m
          z <- raise $ toListList mm
          pureT ()
    )
