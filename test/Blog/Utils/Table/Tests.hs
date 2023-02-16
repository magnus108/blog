module Blog.Utils.Table.Tests
  ( tests,
  )
where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

import Blog.Table
import Blog.Utils.ListZipper
import Control.Monad.Writer
import Data.Function
import Polysemy
import Polysemy.State
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util
import Text.Blaze.Html.Renderer.String

--------------------------------------------------------------------------------

example :: (Member Table r) => Sem r ()
example = do
  tab $ do
    row $ do
      col $ link "a0" "l0"
      col $ link "b0" "l0"
      col $ link "c0" "l0"
    row $ do
      col $ link "a1" "l1"
      col $ link "b1" "l1"
      col $ link "c1" "l1"
    row $ do
      col $ link "a2" "l2"
      col $ link "b2" "l2"
      col $ link "c2" "l2"

tests :: TestTree
tests =
  testGroup "Blog.Utils.Table.Tests" $
    concat
      [ fromAssertions
          "table"
          [ [[("a0", "l0"), ("b0", "l0"), ("c0", "l0")], [("a1", "l1"), ("b1", "l1"), ("c1", "l1")], [("a2", "l2"), ("b2", "l2"), ("c2", "l2")]]
              @=? ( example
                      & toListList
                      & evalState @[(String, String)] []
                      & execState @[[(String, String)]] []
                      & run
                  )
          ]
      ]
