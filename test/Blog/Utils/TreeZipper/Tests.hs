module Blog.Utils.TreeZipper.Tests
  ( tests,
  )
where

import qualified Blog.Utils.RoseTree as R
import qualified Blog.Utils.TreeZipper as TZ
import System.FilePath (splitPath)
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Blog.Utils.TreeZipper.Tests" $
    concat
      [ fromAssertions
          "datum"
          [ "projects/" @=? TZ.datum zipper
          ],
        fromAssertions
          "down"
          [ Just "chair/" @=? TZ.datum <$> TZ.down "chair/" zipper
          ],
        fromAssertions
          "downTo"
          [ Just "door/" @=? TZ.datum <$> TZ.downTo ["kitchen/", "door/"] zipper
          ],
        fromAssertions
          "up"
          [ Just "projects/" @=? TZ.datum <$> (TZ.down "chair/" zipper >>= TZ.up)
          ],
        fromAssertions
          "firstChild"
          [ Just "chair/" @=? TZ.datum <$> (TZ.firstChild zipper)
          ],
        fromAssertions
          "lefts"
          [ [] @=? TZ.lefts zipper,
            Just [] @=? fmap TZ.datum <$> TZ.lefts <$> TZ.down "chair/" zipper,
            Just ["chair/"] @=? fmap TZ.datum <$> TZ.lefts <$> TZ.down "kitchen/" zipper,
            Just ["chair/", "kitchen/"] @=? fmap TZ.datum <$> TZ.lefts <$> TZ.down "lamp/" zipper
          ],
        fromAssertions
          "rights"
          [ [] @=? TZ.rights zipper,
            Just ["kitchen/", "lamp/"] @=? fmap TZ.datum <$> TZ.rights <$> TZ.down "chair/" zipper,
            Just ["lamp/"] @=? fmap TZ.datum <$> TZ.rights <$> TZ.down "kitchen/" zipper,
            Just [] @=? fmap TZ.datum <$> TZ.rights <$> TZ.down "lamp/" zipper
          ],
        fromAssertions
          "children"
          [ ["chair/", "kitchen/", "lamp/"] @=? TZ.datum <$> TZ.children zipper,
            Just ["index.md"] @=? fmap TZ.datum <$> TZ.children <$> TZ.down "chair/" zipper
          ],
        fromAssertions
          "siblings"
          [ Just ["chair/", "kitchen/", "lamp/"] @=? fmap TZ.datum <$> TZ.siblings <$> TZ.down "chair/" zipper
          ]
      ]
  where
    zipper =
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
