module Blog.Utils.Trie.Tests
    ( tests
    ) where

--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
import           System.FilePath                ( splitPath )


--------------------------------------------------------------------------------
import Blog.Utils.Trie

--------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Unit tests"
    [  testCase "level1" $ assertEqual "toStructure" empty (insert (insert empty ["hey","plate"]) ["hey","mate","said"])
    --,  testCase, "level1" $ assertEqual "toStructure" empty (trie project)
    ]
