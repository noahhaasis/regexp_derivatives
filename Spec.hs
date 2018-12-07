import Test.Hspec

import Regex

main :: IO ()
main = hspec $ do
  describe "matching string with language" $ do
    it "matches single letter" $ do
      matchLanguage (Singleton 'c') "c" `shouldBe` True

    it "matches empty word" $ do
      matchLanguage Eps "" `shouldBe` True

    it "doesn't matchLanguage empty language" $ do
      matchLanguage Empty "" `shouldBe` False

    it "matches multiple chars" $ do
      matchLanguage (Cat (Singleton 'a') (Singleton 'b')) "ab" `shouldBe` True

    it "matches the empty string for repetition" $ do
      matchLanguage (Rep (Singleton 'd')) "" `shouldBe` True

    it "matches multiple chars for repetition" $ do
      matchLanguage (Rep (Singleton 'd')) "dddd" `shouldBe` True

    it "matches numbers" $ do
      let digits = map Singleton "0123456789"
          digit = foldr Alt Empty digits
          number = Cat digit (Rep digit)
      matchLanguage number "12981" `shouldBe` True

  describe "matching string with regular expression" $ do
    it "pattern \"a\" matches pattern \"a\"" $ do
      match "a" "a" `shouldBe` True

    it "pattern \"a\" doesn't match \"b\"" $ do
      match "a" "b" `shouldBe` False

    it "pattern \"c\" matches \"c\"" $ do
      match "c" "c" `shouldBe` True

    it "pattern \"c\" doesn't match \"d\"" $ do
      match "c" "d" `shouldBe` False

    it "pattern \"abc\" matches \"abc\"" $ do
      match "abc" "abc" `shouldBe` True

    it "pattern \"dog|cat\" matches \"dog\"" $ do
      match "dog|cat" "dog" `shouldBe` True

    it "pattern \"dog|cat\" matches \"cat\"" $ do
      match "dog|cat" "cat" `shouldBe` True

    it "pattern \"dog|cat\" doesn't match \"ape\"" $ do
      match "dog|cat" "ape" `shouldBe` False

    -- it "pattern \"dog\|cat\" should match \"dog|cat\"" $ do
    --   match "dog\|cat" "dog|cat" `shouldBe` True
