module Test.MatchLineSpec
  ( spec
  )
where

import           Cut.Analyze
import           Test.Hspec

spec :: Spec
spec = describe "Match a line" $ do
  it "should satisfy equality"
    $          getStart "[silencedetect @ 0xad44e0] silence_start: -0.012381"
    `shouldBe` (-0.012381)
  it "Should match the end line as well"
    $          getEnd
                 "[silencedetect @ 0x23a94e0] silence_end: 148.515 | silence_duration: 0.825079"
    `shouldBe` (148.515)
  it "Should match the duration"
    $          getDuration
                 "[silencedetect @ 0x23a94e0] silence_end: 148.515 | silence_duration: 0.825079"
    `shouldBe` 0.825079
