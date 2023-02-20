module Blog.Utils.ForestZipper.Tests
  ( tests,
  )
where

import qualified Blog.Utils.Forest as F
import qualified Blog.Utils.ForestZipper as FZ
import qualified Blog.Utils.RoseTree as R
import qualified Blog.Utils.TreeZipper as TZ
import Data.Function
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
          [ Just "index.md" @=? FZ.datum <$> forestZipper
          ],
        fromAssertions
          "siblings"
          [ Just ["index.md", "cv/", "posts/", "projects/"] @=? fmap TZ.datum <$> (FZ.siblings <$> forestZipper),
            Just ["applicative/", "index.md", "tests/"] @=? fmap TZ.datum <$> (FZ.siblings <$> (forestZipper >>= FZ.forward >>= FZ.forward >>= FZ.down "applicative/")),
            Just ["index.md"] @=? fmap TZ.datum <$> (FZ.siblings <$> (forestZipper >>= FZ.forward >>= FZ.forward >>= FZ.down "applicative/" >>= FZ.down "index.md"))
          ],
        fromAssertions
          "down"
          [ Just "applicative/" @=? FZ.datum <$> (forestZipper >>= FZ.forward >>= FZ.forward >>= FZ.down "applicative/")
          ],
        fromAssertions
          "downTo"
          [ Just "index.md" @=? FZ.datum <$> (forestZipper >>= FZ.forward >>= FZ.forward >>= FZ.downTo ["applicative/", "index.md"]),
            Just "handle/" @=? FZ.datum <$> (forestZipper >>= FZ.forward >>= FZ.forward >>= FZ.forward >>= FZ.downTo ["kitchen/", "door/", "handle/"])
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
          [ Just "posts/" @=? FZ.datum <$> (forestZipper >>= FZ.forward >>= FZ.forward)
          ],
        fromAssertions
          "backward"
          [ Just "cv/" @=? FZ.datum <$> (forestZipper >>= FZ.forward >>= FZ.forward >>= FZ.backward)
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
