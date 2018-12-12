module ConfigSpec where

import Test.Hspec

spec = do
  it "should fail" $
    True `shouldBe` False
