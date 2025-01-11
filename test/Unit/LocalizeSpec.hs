{-# Language OverloadedStrings #-}

module Unit.LocalizeSpec where

import Text.Localize as L

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "lookup" $ do
    it "returns source string if the translation is not found" $
      L.lookup mempty "en" "prova" `shouldBe` "prova"
    it "returns a translated string if the translation is present" $ do
      ts <- loadTranslations [("en_GB", "test/mock-program/en_GB.mo")]
      L.lookup ts "en_GB" "prova" `shouldBe` "test"
