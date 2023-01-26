module Main
    ( main
    ) where
--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import qualified Blog.Utils.Trie.Tests
import qualified Blog.Utils.RoseTree.Tests
import qualified Blog.Utils.ListZipper.Tests
import qualified Blog.Utils.TreeZipper.Tests
import qualified Blog.Utils.ForestZipper.Tests
import qualified Blog.Utils.Table.Tests


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Blog.Utils.Trie.Tests.tests
    , Blog.Utils.RoseTree.Tests.tests
    , Blog.Utils.ListZipper.Tests.tests
    , Blog.Utils.TreeZipper.Tests.tests
    , Blog.Utils.ForestZipper.Tests.tests
    , Blog.Utils.Table.Tests.tests
    ]
