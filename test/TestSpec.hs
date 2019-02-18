{-# LANGUAGE OverloadedStrings #-}

module TestSpec
  ( spec
  ) where

import           Test.Hspec

spec :: Spec
spec = do
  describe "arathemic falls within cultural norms" $ do
    it "equals another one" $
      1 == 1 `shouldBe` True
