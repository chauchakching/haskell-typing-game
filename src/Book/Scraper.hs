module Book.Scraper where

import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Monoid                    ( mempty )
import qualified Data.ByteString.Char8         as B
import qualified Data.Text                     as T
import           Text.HTML.Scalpel              ( scrapeURL
                                                , attrs
                                                , Scraper
                                                , match
                                                -- hlint bug in parsing this operator 
                                                -- https://github.com/ndmitchell/hlint/issues/573
                                                , (@:) 
                                                )
import           Text.Regex.TDFA                ( (=~) )

bookListUrl = "http://gutenberg.net.au/plusfifty-n-z.html"

-- TODO:
-- write ebook urls to file

type Link = String

isEbookLink :: String -> String -> Bool
isEbookLink attrKey attrVal = attrKey == "href" && isEbookUrl attrVal

ebookUrlRegex :: String
ebookUrlRegex = "^/ebooks(.*)txt$"

isEbookUrl :: String -> Bool
isEbookUrl str = str =~ ebookUrlRegex

getBookLinks :: IO ()
getBookLinks = do
  maybeEbookUrls <- scrapBookLinks
  case maybeEbookUrls of 
    Nothing -> putStrLn "scraper parsing error"
    Just urls -> print urls

scrapBookLinks :: IO (Maybe [Link])
scrapBookLinks = scrapeURL bookListUrl bookLinks
 where
  bookLinks :: Scraper String [Link]
  bookLinks = attrs "href" $ "a" @: [match isEbookLink]
