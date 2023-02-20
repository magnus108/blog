module Blog.Utils.Table.Tests
  ( tests,
  )
where

import qualified Blog.Link as Link
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

example :: (Member Table r) => Sem r ()
example = do
  tab $ do
    row $ do
      col $ link (Link.link "a0" "l0")
      col $ link (Link.link "b0" "l0")
      col $ link (Link.link "c0" "l0")
    row $ do
      col $ link (Link.link "a1" "l1")
      col $ link (Link.link "b1" "l1")
      col $ link (Link.link "c1" "l1")
    row $ do
      col $ link (Link.link "a2" "l2")
      col $ link (Link.link "b2" "l2")
      col $ link (Link.link "c2" "l2")

tests :: TestTree
tests =
  testGroup "Blog.Utils.Table.Tests" $
    concat
      [ fromAssertions
          "table"
          [ [ [ Link.link "a0" "l0",
                Link.link "b0" "l0",
                Link.link "c0" "l0"
              ],
              [ Link.link "a1" "l1",
                Link.link "b1" "l1",
                Link.link "c1" "l1"
              ],
              [ Link.link "a2" "l2",
                Link.link "b2" "l2",
                Link.link "c2" "l2"
              ]
            ]
              @=? ( example
                      & toListList
                      & evalState @[Link.Link] []
                      & execState @[[Link.Link]] []
                      & run
                  )
          ]
      ]
