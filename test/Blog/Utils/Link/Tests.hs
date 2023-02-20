module Blog.Utils.Link.Tests
  ( tests,
  )
where

import qualified Blog.Link as Link
import qualified Blog.Utils.RoseTree as R
import qualified Blog.Utils.TreeZipper as TZ
import Data.Function
import Data.Functor
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Blog.Utils.Link.Tests" $
    concat
      [ fromAssertions
          "linkFromTreeZipper"
          [ Link.link "projects/" "projects/"
              @=? ( treeZipper
                      & Link.fromTreeZipper
                  ),
            Just (Link.link "chair/" "projects/chair/")
              @=? ( treeZipper
                      & TZ.down "chair/"
                      <&> Link.fromTreeZipper
                  )
          ]
      ]
  where
    treeZipper =
      TZ.fromRoseTree $
        R.roseTree
          "projects/"
          [ R.roseTree
              "chair/"
              [R.roseTree "index.md" []],
            R.roseTree
              "kitchen/"
              [ R.roseTree
                  "door/"
                  [ R.roseTree
                      "handle/"
                      [R.roseTree "index.md" []]
                  ]
              ],
            R.roseTree "lamp/" []
          ]
