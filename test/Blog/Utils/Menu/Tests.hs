module Blog.Utils.Menu.Tests
  ( tests,
  )
where

import qualified Blog.Link as Link
import qualified Blog.Menu as Menu
import Blog.Table
import qualified Blog.Utils.Forest as F
import qualified Blog.Utils.ForestZipper as FZ
import qualified Blog.Utils.RoseTree as R
import Data.Function
import Data.Functor
import Polysemy
import Polysemy.State
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Blog.Utils.Menu.Tests" $
    fromAssertions
      "showMenu"
      [ [ Link.row
            []
            [ Link.link "cv/" "/cv/",
              Link.link "index.md" "/index.md",
              Link.link "posts/" "/posts/",
              Link.link "projects/" "/projects/"
            ],
          Link.row
            []
            [ Link.link "applicative/" "/posts/applicative/",
              Link.link "index.md" "/posts/index.md",
              Link.link "tests/" "/posts/tests/"
            ]
        ]
          @=? ( Menu.makeMenu project
                  & run
                    . execState @[Link.Row [Link.Link]] []
                    . evalState @[Link.Link] []
                    . toListList
                    . Menu.showMenu "posts/applicative/"
              )
      ]
  where
    project =
      [ "cv/index.md",
        "index.md",
        "posts/index.md",
        "posts/applicative/index.md",
        "posts/tests/index.md",
        "projects/chair/index.md",
        "projects/table/index.md",
        "projects/table/legs/index.md",
        "projects/table/top/index.md",
        "projects/kitchen/door/handle/index.md"
      ]
    forestZipper =
      FZ.fromForest $
        F.forest
          [ R.roseTree "cv/" [R.roseTree "index.md" []],
            R.roseTree "index.md" [],
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
