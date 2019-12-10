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

  it "Should deal with rng output"
    $ getStart "[silencedetect @ 0x10b0280] silence_start: 430.41'peed= 858x"
    `shouldBe` 430.41
  it "Should match the end line as well"
    $          getEnd
                 "[silencedetect @ 0x23a94e0] silence_end: 148.515 | silence_duration: 0.825079"
    `shouldBe` 148.515
  it "Should match the duration"
    $          getDuration
                 "[silencedetect @ 0x23a94e0] silence_end: 148.515 | silence_duration: 0.825079"
    `shouldBe` 0.825079

  it "Should deal with quotes on end as well"
    $          getDuration
                 "[silencedetect @ 0x1900280] silence_end: 1249.91 | silence_duration: 15.7841'"
    `shouldBe` 15.7841

  it "Shouldn't filter this line"
    $          takeOnlyLines
                 "[silencedetect @ 0xcf3280] silence_end: 415.498 | silence_duration: 69.6312'"
    `shouldBe` True

