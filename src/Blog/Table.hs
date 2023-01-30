module Blog.Table
    ( Table
    , TableF
    , col
    , row
    , runTableWriter
    , runTableHTML
    )
where

import qualified Blog.Utils.ListZipper as LZ

import Control.Monad.Writer
import Control.Monad.Free


import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Internal



data TableF next
    = Row (Table ()) next
    | Col String next
    deriving stock (Functor)


type Table a = Free TableF a


row :: Table ()  -> Table ()
row args = liftF (Row args ())


col :: String  -> Table ()
col args = liftF (Col args ())


runTableWriter' :: TableF a -> Writer [[String]] a
runTableWriter' cmd = case cmd of
                    Row args next -> do
                        tell [(concat (execWriter (runTableWriter args)))]
                        pure next
                    Col args next -> do
                        tell [[args]]
                        pure next


runTableHTML' :: TableF a -> MarkupM a
runTableHTML' cmd = case cmd of
                    Row args next -> do
                        H.ul (runTableHTML args)
                        pure next
                    Col args next -> do
                        H.li (H.toHtml args)
                        pure next



runTableWriter :: Table a -> Writer [[String]] a
runTableWriter = foldFree runTableWriter'

runTableHTML :: Table a -> MarkupM a
runTableHTML = foldFree runTableHTML'
