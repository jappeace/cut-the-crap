module Test.TestSpec
  ( spec
  )
where

import           Test.Hspec

one :: Int
one = 1

spec :: Spec
spec =
  describe "The sanity of our test setup"
    $          it "should satisfy equality"
    $          one
    `shouldBe` 1
