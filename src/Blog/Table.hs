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
  Link :: String -> String -> Table m ()
  Col :: m () -> Table m ()
  Row :: m () -> Table m ()
  Tab :: m () -> Table m ()

makeSem ''Table

table :: (Member Table r) => [LZ.ListZipper ((String, String))] -> Sem r ()
table rows = tab $ forM_ rows $ \rowData -> do
  row $ do
    forM_ rowData $ \(colData1, colData2) -> do
      col $ link (colData1) (colData2)

toHtml :: (Member (Embed MarkupM) r, Member (State [(String, String)]) r, Member (State [[(String, String)]]) r) => Sem (Table ': r) a -> Sem r a
toHtml =
  interpretH
    ( \case
        Link o1 o2 -> do
          modify @[(String, String)] (++ [(o1, o2)])
          pureT ()
        Col m -> do
          mm <- runT m
          z <- raise $ toHtml mm
          pureT ()
        Row m -> do
          mm <- runT m
          z <- raise $ toHtml mm
          row <- get @[(String, String)]
          modify @[[(String, String)]] (++ [row])
          put @[(String, String)] []
          pureT ()
        Tab m -> do
          mm <- runT m
          z <- raise $ toHtml mm
          rows <- get @[[(String, String)]]
          embed $ forM_ rows $ \row -> do
            H.ul $ do
              forM_ row $ \(col1, col2) -> do
                H.li $ H.a ! A.href (stringValue col1) $ H.toHtml col2
          pureT ()
    )

toListList :: (Member (State [(String, String)]) r, Member (State [[(String, String)]]) r) => Sem (Table ': r) a -> Sem r a
toListList =
  interpretH
    ( \case
        Link o1 o2 -> do
          modify @[(String, String)] (++ [(o1, o2)])
          pureT ()
        Col m -> do
          mm <- runT m
          z <- raise $ toListList mm
          pureT ()
        Row m -> do
          mm <- runT m
          z <- raise $ toListList mm
          row <- get @[(String, String)]
          modify @[[(String, String)]] (++ [row])
          put @[(String, String)] []
          pureT ()
        Tab m -> do
          mm <- runT m
          z <- raise $ toListList mm
          pureT ()
    )
