module Blog.Utils.ListZipper.Tests
  ( tests,
  )
where

import qualified Blog.Utils.ListZipper as LZ
import Control.Comonad
import Data.Functor
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Blog.Utils.ListZipper.Tests" $
    concat
      [ fromAssertions
          "fromList"
          [ Nothing
              @=? ( LZ.fromList ([] :: [Int])
                      <&> extract
                  ),
            Just 1
              @=? ( LZ.fromList [1, 2, 3]
                      <&> extract
                  )
          ],
        fromAssertions
          "setFocus"
          [ Just [1, 5, 3]
              @=? ( LZ.fromList [1, 2, 3]
                      >>= LZ.forward
                      <&> LZ.setFocus 5
                      <&> LZ.toList
                  )
          ],
        fromAssertions
          "toList"
          [ Just [1, 2, 3]
              @=? ( LZ.fromList [1, 2, 3]
                      >>= LZ.forward
                      <&> LZ.toList
                  )
          ],
        fromAssertions
          "forward"
          [ Nothing
              @=? ( LZ.fromList [1, 2, 3]
                      >>= LZ.forward
                      >>= LZ.forward
                      >>= LZ.forward
                      <&> extract
                  ),
            Just 2
              @=? ( LZ.fromList [1, 2, 3]
                      >>= LZ.forward
                      <&> extract
                  )
          ],
        fromAssertions
          "backward"
          [ Nothing
              @=? ( LZ.fromList [1, 2, 3]
                      >>= LZ.backward
                      <&> extract
                  ),
            Just 2
              @=? ( LZ.fromList [1, 2, 3]
                      >>= LZ.forward
                      >>= LZ.forward
                      >>= LZ.backward
                      <&> extract
                  )
          ],
        fromAssertions
          "lefts"
          [ Just []
              @=? ( LZ.fromList [1, 2, 3]
                      <&> LZ.lefts
                  ),
            Just
              [1, 2]
              @=? ( LZ.fromList [1, 2, 3]
                      >>= LZ.forward
                      >>= LZ.forward
                      <&> LZ.lefts
                  )
          ],
        fromAssertions
          "rights"
          [ Just []
              @=? ( LZ.fromList [1, 2, 3]
                      >>= LZ.forward
                      >>= LZ.forward
                      <&> LZ.rights
                  ),
            Just [2, 3]
              @=? ( LZ.fromList [1, 2, 3]
                      <&> LZ.rights
                  )
          ]
      ]
