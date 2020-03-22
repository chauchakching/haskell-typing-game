import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Book.Scraper                   ( isEbookUrl )

main :: IO ()
main = hspec $ do
  describe "ebook url regex" $ do
    describe "/ebooks06/0602471.txt" $ do
      it "return true" $ do
        isEbookUrl "/ebooks06/0602471.txt" `shouldBe` True

    describe "/ebooks06/0602471.html" $ do
      it "return false" $ do
        isEbookUrl "/ebooks06/0602471.html" `shouldBe` False
