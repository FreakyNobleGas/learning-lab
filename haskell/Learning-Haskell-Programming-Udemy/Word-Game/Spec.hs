--
-- Name: Nicholas Quinn
--
-- Description: Unit Testing for Word Game using Hspec
--

import Test.Hspec
import WordGameLib
import GridData

main :: IO()
main = hspec $ do
    describe "formatGrid" $ do
        it "Should concatenate every line with a newline" $ do
            (formatGrid ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"

    describe "findword" $ do
        it "Should find words that exist on the Grid" $ do
            findWord grid "HASKELL" `shouldBe` Just "HASKELL"
            findWord grid "PERL" `shouldBe` Just "PERL"
        it "Should not find words that do not exist on the Grid" $ do
            findWord grid "DOESNOTEXIST" `shouldBe` Nothing

    describe "findWords" $ do
        it "Should find all the words that exist on the Grid" $ do
            findWords grid languages `shouldBe` languages
        it "Should not finall words that do not exist on the Grid" $ do
            findWords grid ["DOES", "NOT", "EXIST"] `shouldBe` []