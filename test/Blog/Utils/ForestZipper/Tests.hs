module Blog.Utils.ForestZipper.Tests
    ( tests
    ) where

--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
import           System.FilePath                ( splitPath )


--------------------------------------------------------------------------------
import           TestSuite.Util


import qualified Blog.Utils.RoseTree as R
import qualified Blog.Utils.TreeZipper as TZ
import qualified Blog.Utils.ForestZipper as FZ
--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Blog.Utils.ForestZipper.Tests" $ concat
    [  fromAssertions "datum"
        [ Just "index.md" @=? FZ.datum <$> forestZipper
        ]

    ,  fromAssertions "down"
        [ Just "applicative/" @=? FZ.datum <$> (forestZipper2 >>= FZ.down "applicative/")
        ]

    ,  fromAssertions "up"
        [ Just "posts/" @=? FZ.datum <$> (forestZipper2 >>= FZ.down "applicative/" >>= FZ.up)
        ]
    ]
    where
        forestZipper = FZ.fromForest $ R.forest
                    [ R.roseTree "index.md" []
                    , R.roseTree "cv/" [R.roseTree "index.md" []]
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
                                    ]
                                , R.roseTree "table/"
                                    [ R.roseTree "index.md" []
                                    , R.roseTree "legs/"
                                        [ R.roseTree "index.md" []]
                                        , R.roseTree "top/"
                                            [ R.roseTree "index.md" []]
                                    ]
                                ]
                    ]

        forestZipper2 = FZ.fromForest $ R.forest
                    [ R.roseTree "posts/"
                                [ R.roseTree "applicative/"
                                    [ R.roseTree "index.md" []]
                                , R.roseTree "index.md" []
                                , R.roseTree "tests/"
                                    [ R.roseTree "index.md" []]
                                ]
                    , R.roseTree "index.md" []
                    , R.roseTree "cv/" [R.roseTree "index.md" []]
                    , R.roseTree "posts/"
                                [ R.roseTree "applicative/"
                                    [ R.roseTree "index.md" []]
                                , R.roseTree "index.md" []
                                , R.roseTree "tests/"
                                    [ R.roseTree "index.md" []]
                                ]
                    ]

