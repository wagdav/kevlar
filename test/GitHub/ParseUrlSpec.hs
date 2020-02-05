module GitHub.ParseUrlSpec where

import Kevlar.GitHub.ParseUrl
import Test.Hspec

kevlar = Just ("wagdav", "kevlar")

spec = describe "parseUrl" $ do
  it "parses SSH address" $
    parseUrl "git@github.com:wagdav/kevlar" `shouldBe` kevlar
  it "parses HTTPS address" $
    parseUrl "https://github.com/wagdav/kevlar" `shouldBe` kevlar
  it "ignores .git suffix" $
    parseUrl "https://github.com/wagdav/kevlar.git" `shouldBe` kevlar
  it "fails with non-GitHub URL" $
    parseUrl "https://example.com/wagdav/kevlar.git" `shouldBe` Nothing
