module Blog.Utils.ForestZipper.Tests
  ( tests,
  )
where

import qualified Blog.Utils.Forest as F
import Blog.Utils.ForestZipper (ancestors)
import qualified Blog.Utils.ForestZipper as FZ
import qualified Blog.Utils.RoseTree as R
import qualified Blog.Utils.TreeZipper as TZ
import Data.Function
import Data.Functor
import System.FilePath (splitPath)
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Blog.Utils.ForestZipper.Tests" $
    concat
      [ fromAssertions
          "datum"
          [ Just "index.md"
              @=? ( forestZipper
                      <&> FZ.datum
                  )
          ],
        fromAssertions
          "siblings"
          [ Just ["index.md", "cv/", "posts/", "projects/"]
              @=? ( forestZipper
                      <&> FZ.siblings
                      <&> fmap TZ.datum
                  ),
            Just ["applicative/", "index.md", "tests/"]
              @=? ( forestZipper
                      >>= FZ.forward
                      >>= FZ.forward
                      >>= FZ.down "applicative/"
                      <&> FZ.siblings
                      <&> fmap TZ.datum
                  ),
            Just ["index.md"]
              @=? ( forestZipper
                      >>= FZ.forward
                      >>= FZ.forward
                      >>= FZ.down "applicative/"
                      >>= FZ.down "index.md"
                      <&> FZ.siblings
                      <&> fmap TZ.datum
                  )
          ],
        fromAssertions
          "down"
          [ Just "applicative/"
              @=? ( forestZipper
                      >>= FZ.forward
                      >>= FZ.forward
                      >>= FZ.down "applicative/"
                      <&> FZ.datum
                  )
          ],
        fromAssertions
          "downTo"
          [ Just "index.md"
              @=? ( forestZipper
                      >>= FZ.forward
                      >>= FZ.forward
                      >>= FZ.downTo ["applicative/", "index.md"]
                      <&> FZ.datum
                  ),
            Just "handle/"
              @=? ( forestZipper
                      >>= FZ.forward
                      >>= FZ.forward
                      >>= FZ.forward
                      >>= FZ.downTo ["kitchen/", "door/", "handle/"]
                      <&> FZ.datum
                  )
          ],
        fromAssertions
          "up"
          [ Just "posts/"
              @=? ( forestZipper
                      >>= FZ.forward
                      >>= FZ.forward
                      >>= FZ.down "applicative/"
                      >>= FZ.up
                      & fmap FZ.datum
                  )
          ],
        fromAssertions
          "forward"
          [ Just "posts/"
              @=? ( forestZipper
                      >>= FZ.forward
                      >>= FZ.forward
                      <&> FZ.datum
                  )
          ],
        fromAssertions
          "backward"
          [ Just "cv/"
              @=? ( forestZipper
                      >>= FZ.forward
                      >>= FZ.forward
                      >>= FZ.backward
                      <&> FZ.datum
                  )
          ],
        fromAssertions
          "moveTo"
          [ Just "index.md"
              @=? ( forestZipper
                      >>= FZ.moveTo ["posts/", "applicative/", "index.md"]
                      <&> FZ.datum
                  ),
            Just "handle/"
              @=? ( forestZipper
                      >>= FZ.moveTo ["projects/", "kitchen/", "door/", "handle/"]
                      <&> FZ.datum
                  )
          ],
        fromAssertions
          "ancestors"
          [ Just
              [ ["posts/", "posts/", "posts/", "posts/"],
                ["applicative/", "index.md", "tests/"],
                ["index.md"]
              ]
              @=? ( forestZipper
                      >>= FZ.moveTo ["posts/", "applicative/", "index.md"]
                      <&> FZ.ancestors
                      <&> fmap (fmap TZ.datum)
                  ),
            Just
              [ ["projects/", "projects/", "projects/", "projects/"],
                ["chair/", "kitchen/", "table/"],
                ["door/"],
                ["handle/"]
              ]
              @=? ( forestZipper
                      >>= FZ.moveTo ["projects/", "kitchen/", "door/", "handle/"]
                      <&> FZ.ancestors
                      <&> fmap (fmap TZ.datum)
                  )
          ]
      ]
  where
    forestZipper =
      FZ.fromForest $
        F.forest
          [ R.roseTree "index.md" [],
            R.roseTree "cv/" [R.roseTree "index.md" []],
            R.roseTree
              "posts/"
              [ R.roseTree
                  "applicative/"
                  [R.roseTree "index.md" []],
                R.roseTree "index.md" [],
                R.roseTree
                  "tests/"
                  [R.roseTree "index.md" []]
              ],
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
                R.roseTree
                  "table/"
                  [ R.roseTree "index.md" [],
                    R.roseTree
                      "legs/"
                      [R.roseTree "index.md" []],
                    R.roseTree
                      "top/"
                      [R.roseTree "index.md" []]
                  ]
              ]
          ]
