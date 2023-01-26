module Blog.Utils.Table.Tests
    ( tests
    ) where

--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit

import Text.Blaze.Html.Renderer.String

--------------------------------------------------------------------------------
import           TestSuite.Util

import Blog.Table
import Blog.Utils.ListZipper
import Control.Monad.Writer

--------------------------------------------------------------------------------

exampleScript :: Table ()
exampleScript = do
    mkTable [ listZipper ["a0"] "b0" ["c0"]
            , listZipper [] "a1" ["b1","c1"]
            , listZipper ["a2","b2"] "c2" []
            ]


tests :: TestTree
tests = testGroup "Blog.Utils.Table.Tests" $ concat
    [  fromAssertions "table"
        [ [ ["a0", "*b0*","c0"]
          , ["*a1*","b1","c1"]
          , ["a2","b2","*c2*"]
          ]
          @=? execWriter (runTableWriter exampleScript)
        ]
    ]
