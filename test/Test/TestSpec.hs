module Test.TestSpec
  ( spec
  ) where

import           Test.Hspec

one :: Int
one = 1

spec :: Spec
spec = do
  describe "The sanity of our test setup" $ do
    it "should satisfy equality" $ one `shouldBe` 1
