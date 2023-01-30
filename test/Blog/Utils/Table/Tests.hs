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
        row $ do 
                col "a0"
                col "b0"
                col "c0"
        row $ do 
                col "a1"
                col "b1"
                col "c1"
        row $ do 
                col "a2"
                col "b2"
                col "c2"


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
