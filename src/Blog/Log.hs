module Blog.Log
    ( mkTable
    , mainer
    )
where

import Debug.Trace

import Data.Function
import Polysemy
import Polysemy.Writer
import Polysemy.State

import Control.Monad
import qualified Blog.Utils.ListZipper as LZ
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal

--MkTable :: [LZ.ListZipper String] -> Table m ()
data Table m a where
  MkTable :: [[String]] -> Table m ()

makeSem ''Table

data Row m a where
  MkRow :: LZ.ListZipper String -> Row m [String]

makeSem ''Row

data Col m a where
  MkCol :: String -> Col m ()

makeSem ''Col

myBusinessFunction :: (Member Row r, Member Table r) => [LZ.ListZipper String] -> Sem r ()
myBusinessFunction xs = do
  rows <- forM xs myBusinessFunction2
  mkTable rows

myBusinessFunction2 :: Member Row r => LZ.ListZipper String -> Sem r [String]
myBusinessFunction2 x = do
  mkRow x

--tableToHtml :: Member (Embed MarkupM) r => Sem (Table ': r) a -> Sem r a
--tableToHtml = interpret (\(MkTable table) -> embed $ do
 --                           forM_ table (\rowData -> H.ul $ do
  --                                              forM_ rowData $ (\colData -> do
   --                                                                 H.li $ H.toHtml colData
    --                                                            )
     --                                   )
      --                      )


tableToWriter :: (Member (Writer [[String]]) r) => Sem (Table ': r) a -> Sem r a
tableToWriter = interpret (\(MkTable args) -> do
        tell args
    )

rowToWriter :: Sem (Row ': r) a -> Sem r a
rowToWriter = interpret (\(MkRow args) -> do
        let gg = LZ.toList (LZ.mapFocus (\x -> "*"++x++"*" ) args)
        return gg
    )

colToWriter :: Member (Writer String) r => Sem (Col ': r) a -> Sem r a
colToWriter = interpret (\(MkCol args) -> do
        tell args
    )


mainer :: IO ()
mainer = do
    --let gg = myBusinessFunction []
     --               & tableToHtml
      --              & runM @MarkupM
      --
    let gg = [ LZ.listZipper ["a0"] "a0" ["c0"]
             , LZ.listZipper ["a1"] "a1" ["c1"]
             , LZ.listZipper ["a2"] "a2" ["c2"]
             ]

    let (t, r) = myBusinessFunction gg
                        & tableToWriter
                        & rowToWriter
                        & runWriter @[[String]]
                        & run
    putStrLn (show t)
    pure ()
