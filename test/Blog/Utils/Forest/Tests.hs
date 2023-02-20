module Blog.Utils.Forest.Tests
  ( tests,
  )
where

import qualified Blog.Utils.Forest as F
import qualified Blog.Utils.RoseTree as R
import qualified Blog.Utils.Trie as T
import Data.Function
import System.FilePath (splitPath)
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Blog.Utils.Forest.Tests" $
    concat
      [ fromAssertions
          "forest"
          [ forest @=? (T.trie project & F.fromTrie)
          ]
      ]
  where
    project =
      splitPath
        <$> [ "index.md",
              "cv/index.md",
              "posts/index.md",
              "posts/applicative/index.md",
              "posts/tests/index.md",
              "projects/chair/index.md",
              "projects/table/index.md",
              "projects/table/legs/index.md",
              "projects/table/top/index.md",
              "projects/kitchen/door/handle/index.md"
            ]
    forest =
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
