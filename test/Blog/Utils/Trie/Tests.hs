module Blog.Utils.Trie.Tests
    ( tests
    ) where

--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
import           System.FilePath                ( splitPath )


--------------------------------------------------------------------------------
import           TestSuite.Util

import Blog.Utils.Trie

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Blog.Utils.Trie.Tests" $ concat
    [  fromAssertions "insert"
        [  insert (insert empty ["A","C","D"]) ["A","B"] @=? insert (insert empty ["A","B"]) ["A","C","D"]
        ]
    ]
