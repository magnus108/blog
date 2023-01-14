import Test.Tasty
import Test.Tasty.HUnit


import qualified Blog.Menu as M
import qualified Blog.Utils.TreeZipper as TZ
import qualified Blog.Utils.RoseTree as RT
import Data.Maybe (fromJust)


import qualified Blog.Utils.Trie.Tests


main :: IO ()
main = do
    defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [unitTests, Blog.Utils.Trie.Tests.tests]



project :: [FilePath]
project =
    [ "index.md"
    , "cv/index"
    , "posts/index"
    , "posts/applicative/index"
    , "posts/tests/index"
    , "projects/chair/index.md"
    , "projects/table/index.md"
    , "projects/table/legs/index.md"
    , "projects/table/top/index.md"
    , "projects/kitchen/door/handle/index.md"
    ]


projectRoseTree :: RT.RoseTree FilePath
projectRoseTree = RT.roseTree "index.md"
                        [ RT.roseTree "cv"
                                [ RT.roseTree "index.md" [] ]
                        , RT.roseTree "posts"
                                [ RT.roseTree "index.md" []
                                , RT.roseTree "applicative"
                                        [ RT.roseTree "index.md" [] ]
                                , RT.roseTree "tests"
                                        [ RT.roseTree "index.md" [] ]
                                ]
                        , RT.roseTree "projects"
                                [ RT.roseTree "chair"
                                        [ RT.roseTree "index.md" [] ]
                                , RT.roseTree "table"
                                        [ RT.roseTree "index.md" []
                                        , RT.roseTree "legs"
                                                [ RT.roseTree "index.md" [] ]
                                        , RT.roseTree "top"
                                                [ RT.roseTree "index.md" [] ]
                                        ]
                                , RT.roseTree "kitchen"
                                        [ RT.roseTree "door"
                                            [ RT.roseTree "handle"
                                                [ RT.roseTree "index.md" [] ]
                                            ]
                                        ]
                                ]
                        ]

projectTreeZipper :: TZ.TreeZipper FilePath
projectTreeZipper = TZ.fromRoseTree projectRoseTree


level1 :: RT.RoseTree M.Cols
level1 = RT.roseTree (M.selected "index.md") 
                            [ RT.roseTree (M.unselected "cv") []  --- LISTZIpper -- dog ikke altid
                            , RT.roseTree (M.unselected "posts") []
                            , RT.roseTree (M.unselected "projects") []
                            ]


cv :: TZ.TreeZipper FilePath
cv = fromJust (TZ.down "cv" projectTreeZipper)

cvView :: RT.RoseTree M.Cols
cvView = RT.roseTree (M.selected "index.md")
                            [ RT.roseTree (M.selected "cv")
                                    [ RT.roseTree (M.unselected "index.md") []
                                    ]
                            , RT.roseTree (M.unselected "posts") []
                            , RT.roseTree (M.unselected "projects") []
                            ]



tests' :: TZ.TreeZipper FilePath
tests' = fromJust ((TZ.down "tests") =<< (TZ.down "posts" projectTreeZipper))

testsView :: RT.RoseTree M.Cols
testsView = RT.roseTree (M.selected "index.md")
                            [ RT.roseTree (M.unselected "cv") []
                            , RT.roseTree (M.selected "posts") 
                                [ RT.roseTree (M.unselected "index.md") []
                                , RT.roseTree (M.unselected "applicative") []
                                , RT.roseTree (M.selected "tests")
                                        [ RT.roseTree (M.unselected "index.md") [] ]
                                ]
                            , RT.roseTree (M.unselected "projects") []
                            ]


projects' :: TZ.TreeZipper FilePath
projects' = fromJust (TZ.down "projects" projectTreeZipper)


projectsView :: RT.RoseTree M.Cols
projectsView = RT.roseTree (M.selected "index.md")
                            [ RT.roseTree (M.unselected "cv") []
                            , RT.roseTree (M.unselected "posts") []
                            , RT.roseTree (M.selected "projects") []
                            ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [  testCase "level1" $ assertEqual "isCols" level1 (M.cols projectTreeZipper)
    ,  testCase "cv" $ assertEqual "isCols"  cvView (M.cols cv)
    ,  testCase "tests" $ assertEqual "isCols"  testsView (M.cols tests')
    ]
