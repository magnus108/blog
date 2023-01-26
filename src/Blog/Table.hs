module Blog.Table
    ( Table
    , TableF
    , mkTable
    , runTableWriter
    , runTableHTML
    , runTableIO
    )
where

import qualified Blog.Utils.ListZipper as LZ

import Control.Monad.Writer
import Control.Monad.Free


import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Text.Blaze.Internal

data TableF next
    = TableF [LZ.ListZipper String] next
    deriving stock (Functor)


type Table a = Free TableF a


mkTable :: [ LZ.ListZipper String]  -> Table ()
mkTable args = liftF (TableF args ())


runTableWriter' :: TableF a -> Writer [[String]] a
runTableWriter' cmd = case cmd of
                   TableF args next -> do
                       tell (LZ.toList . (LZ.mapFocus (\x -> "*"++x++"*")) <$> args)
                       pure next


runTableIO' :: TableF a -> IO a
runTableIO' cmd = case cmd of
                   TableF args next -> do
                       forM_ args (putStrLn . unwords . LZ.toList)
                       pure next


runTableHTML' :: TableF a -> MarkupM a
runTableHTML' cmd = case cmd of
                   TableF args next -> do
                       forM_ args (\row -> ul $ forM_ row (li . toHtml))
                       pure next


runTableIO :: Table a -> IO a
runTableIO = foldFree runTableIO'


runTableWriter :: Table a -> Writer [[String]] a
runTableWriter = foldFree runTableWriter'


runTableHTML :: Table a -> MarkupM a
runTableHTML = foldFree runTableHTML'
