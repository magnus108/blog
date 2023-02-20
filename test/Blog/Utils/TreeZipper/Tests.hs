module Blog.Utils.TreeZipper.Tests
  ( tests,
  )
where

import qualified Blog.Utils.RoseTree as R
import qualified Blog.Utils.TreeZipper as TZ
import Data.Functor
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
          [ Just "chair/"
              @=? ( TZ.down "chair/" zipper
                      <&> TZ.datum
                  )
          ],
        fromAssertions
          "downTo"
          [ Just "door/"
              @=? ( TZ.downTo ["kitchen/", "door/"] zipper
                      <&> TZ.datum
                  )
          ],
        fromAssertions
          "up"
          [ Just "projects/"
              @=? ( TZ.down "chair/" zipper
                      >>= TZ.up
                      <&> TZ.datum
                  )
          ],
        fromAssertions
          "parents"
          [ Just ["projects/", "chair/", "index.md"]
              @=? ( TZ.downTo ["chair/", "index.md"] zipper
                      <&> TZ.parents
                      <&> fmap TZ.datum
                  )
          ],
        fromAssertions
          "firstChild"
          [ Just "chair/"
              @=? ( TZ.firstChild zipper
                      <&> TZ.datum
                  )
          ],
        fromAssertions
          "lefts"
          [ [] @=? TZ.lefts zipper,
            Just []
              @=? ( TZ.down "chair/" zipper
                      <&> TZ.lefts
                      <&> fmap TZ.datum
                  ),
            Just ["chair/"]
              @=? ( TZ.down "kitchen/" zipper
                      <&> TZ.lefts
                      <&> fmap TZ.datum
                  ),
            Just ["chair/", "kitchen/"]
              @=? ( TZ.down "lamp/" zipper
                      <&> TZ.lefts
                      <&> fmap TZ.datum
                  )
          ],
        fromAssertions
          "rights"
          [ [] @=? TZ.rights zipper,
            Just ["kitchen/", "lamp/"]
              @=? ( TZ.down "chair/" zipper
                      <&> TZ.rights
                      <&> fmap TZ.datum
                  ),
            Just ["lamp/"]
              @=? ( TZ.down "kitchen/" zipper
                      <&> TZ.rights
                      <&> fmap TZ.datum
                  ),
            Just []
              @=? ( TZ.down "lamp/" zipper
                      <&> TZ.rights
                      <&> fmap TZ.datum
                  )
          ],
        fromAssertions
          "children"
          [ ["chair/", "kitchen/", "lamp/"]
              @=? ( TZ.children zipper
                      <&> TZ.datum
                  ),
            Just ["index.md"]
              @=? ( TZ.down "chair/" zipper
                      <&> TZ.children
                      <&> fmap TZ.datum
                  )
          ],
        fromAssertions
          "siblings"
          [ Just ["chair/", "kitchen/", "lamp/"]
              @=? ( TZ.down "chair/" zipper
                      <&> TZ.siblings
                      <&> fmap TZ.datum
                  )
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
