module Book.UtilSpec where

import           Test.Hspec
import           Book.Util                      ( isSentenceTerminator
                                                , takeFirstSentence
                                                )

main :: IO ()
main = hspec $ do
  describe "isSentenceTerminator" $ do
    describe "a" $ 
      it "False" $ do
        isSentenceTerminator 'a' `shouldBe` False

    describe "." $ do
      it "True" $ do
        isSentenceTerminator '.' `shouldBe` True

  describe "takeFirstSentence" $ do
    it "should take the first sentence" $ do
      takeFirstSentence "Hello World. Goodbye John!" `shouldBe` "Hello World."
