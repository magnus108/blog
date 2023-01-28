module Blog.Utils.Menu.Tests
    ( tests
    ) where

--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit

import Text.Blaze.Html.Renderer.String

--------------------------------------------------------------------------------
import           TestSuite.Util

import Blog.Table
import qualified Blog.Menu as Menu
import qualified Blog.Utils.RoseTree as R
import qualified Blog.Utils.TreeZipper as TZ
import qualified Blog.Utils.ForestZipper as FZ
import Blog.Utils.ListZipper
import Control.Monad.Writer

-------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Blog.Utils.Menu.Tests" $ concat
    [  fromAssertions "makeMenu"
        [ forestZipper @=? (Menu.toForestZipper <$> (Menu.makeMenu project))
        ]
    ]
        where
            project =
                [ "cv/index.md"
                , "index.md"
                , "posts/index.md"
                , "posts/applicative/index.md"
                , "posts/tests/index.md"
                , "projects/chair/index.md"
                , "projects/table/index.md"
                , "projects/table/legs/index.md"
                , "projects/table/top/index.md"
                , "projects/kitchen/door/handle/index.md"
                ]
            forestZipper = FZ.fromForest $ R.forest
                    [ R.roseTree "cv/" [R.roseTree "index.md" []]
                    , R.roseTree "index.md" []
                    , R.roseTree "posts/"
                                [ R.roseTree "applicative/"
                                    [ R.roseTree "index.md" []]
                                , R.roseTree "index.md" []
                                , R.roseTree "tests/"
                                    [ R.roseTree "index.md" []]
                                ]
                    , R.roseTree "projects/"
                                [ R.roseTree "chair/"
                                    [ R.roseTree "index.md" []]
                                , R.roseTree "kitchen/"
                                    [ R.roseTree "door/"
                                        [ R.roseTree "handle/"
                                            [ R.roseTree "index.md" []]
                                        ]
                                    ] , R.roseTree "table/"
                                    [ R.roseTree "index.md" []
                                    , R.roseTree "legs/"
                                        [ R.roseTree "index.md" []]
                                        , R.roseTree "top/"
                                            [ R.roseTree "index.md" []]
                                    ]
                                ]
                    ]
