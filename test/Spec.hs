module Main
  ( main,
  )
where

import qualified Blog.Utils.Forest.Tests
import qualified Blog.Utils.ForestZipper.Tests
import qualified Blog.Utils.Link.Tests
import qualified Blog.Utils.ListZipper.Tests
import qualified Blog.Utils.Menu.Tests
import qualified Blog.Utils.RoseTree.Tests
import qualified Blog.Utils.Table.Tests
import qualified Blog.Utils.TreeZipper.Tests
import qualified Blog.Utils.Trie.Tests
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Blog.Utils.Trie.Tests.tests,
        Blog.Utils.RoseTree.Tests.tests,
        Blog.Utils.ListZipper.Tests.tests,
        Blog.Utils.TreeZipper.Tests.tests,
        Blog.Utils.ForestZipper.Tests.tests,
        Blog.Utils.Table.Tests.tests,
        Blog.Utils.Menu.Tests.tests,
        Blog.Utils.Forest.Tests.tests,
        Blog.Utils.Link.Tests.tests
      ]
