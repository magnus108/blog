module Blog.Utils.ListZipper.Tests
    ( tests
    ) where

--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe

--------------------------------------------------------------------------------
import           TestSuite.Util


import qualified Blog.Utils.ListZipper as LZ
--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Blog.Utils.ListZipper.Tests" $ concat
    [  fromAssertions "fromList"
        [ Nothing @=? LZ.focus <$> LZ.fromList ([] :: [Int])
        , Just 1 @=? LZ.focus <$> LZ.fromList [1,2,3]
        ]

    ,  fromAssertions "forward"
        [ Nothing @=? LZ.focus <$> (LZ.fromList [1,2,3] >>= LZ.forward >>= LZ.forward >>= LZ.forward)
        , Just 2 @=? LZ.focus <$> (LZ.fromList [1,2,3] >>= LZ.forward)
        ]

    ,  fromAssertions "backward"
        [ Nothing @=? LZ.focus <$> (LZ.fromList [1,2,3] >>= LZ.backward)
        , Just 2 @=? LZ.focus <$> (LZ.fromList [1,2,3] >>= LZ.forward >>= LZ.forward >>= LZ.backward)
        ]

    ,  fromAssertions "lefts"
        [ Just [] @=? LZ.lefts <$> LZ.fromList [1,2,3]
        , Just [1,2] @=? LZ.lefts <$> (LZ.fromList [1,2,3] >>= LZ.forward >>= LZ.forward)
        ]

    ,  fromAssertions "rights"
        [ Just [] @=? LZ.rights <$> (LZ.fromList [1,2,3] >>= LZ.forward >>= LZ.forward)
        , Just [2,3] @=? LZ.rights <$> LZ.fromList [1,2,3]
        ]
    ]
