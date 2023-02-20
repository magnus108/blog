module Blog.Utils.RoseTree.Tests
  ( tests,
  )
where

import qualified Blog.Utils.RoseTree as R
import Data.Function
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Blog.Utils.RoseTree.Tests" $
    concat
      [ fromAssertions
          "children"
          [ [R.leaf "b", R.leaf "c"] @=? (R.roseTree "a" [R.leaf "b", R.leaf "c"] & R.children)
          ],
        fromAssertions
          "datum"
          [ "a" @=? (R.roseTree "a" [R.leaf "b", R.leaf "c"] & R.datum)
          ],
        fromAssertions
          "leaf"
          [ [] @=? (R.leaf "a" & R.children)
          ]
      ]
