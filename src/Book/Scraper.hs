module Book.Scraper where

import           Book.Util                      ( pullStdGen
                                                , fetchPlainText
                                                , takeFirstSentence
                                                )

import qualified Data.List                     as L
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Monoid                    ( mempty )

import           System.Random                  ( RandomGen
                                                , randomR
                                                )
import qualified Data.ByteString.Lazy          as B
import qualified Data.Text                     as T
import           Text.HTML.Scalpel              ( scrapeURL
                                                , attrs
                                                , Scraper
                                                , Selector
                                                , match
                                                -- hlint bug in parsing this operator 
                                                -- https://github.com/ndmitchell/hlint/issues/573
                                                , (@:)
                                                , texts
                                                , tagSelector
                                                )
import           Text.Regex.TDFA                ( (=~) )
import           Data.Aeson                     ( decodeFileStrict )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           System.Directory               ( doesFileExist )

-- TODO:
-- random book

boolListWebsite = "http://gutenberg.net.au"
bookListUrl = boolListWebsite ++ "/plusfifty-n-z.html"
ebookLinksFilename = "ebook-links.json"
ebookUrlRegex = "^/ebooks(.*)html$" :: String

minParagraphLength = 5
paragraphLengthVariation = 10

type Link = String

pickParagraphs :: Int -> Int -> [T.Text] -> IO T.Text
pickParagraphs charCount remainingRetry xs = do
  g <- pullStdGen
  return $ pickParagraphs' g charCount remainingRetry "" xs

pickParagraphs'
  :: RandomGen g => g -> Int -> Int -> T.Text -> [T.Text] -> T.Text
pickParagraphs' randomGen charCount remainingRetry pp xs
  |
    -- finish
    remainingRetry <= 0             = pp
  | T.length pp > charCount - paragraphLengthVariation = pp
  | length xs == 0                  = pp
  |
    -- paragraph cannot be too short
    T.length p < minParagraphLength = tryAgain
  |
    -- take first sentence of long paragraph
    T.length p > maxLength          = T.append pp $ takeFirstSentence p
  |
    -- OK, find remaining paragraphs
    otherwise = pickParagraphs' randomGen' charCount' remainingRetry pp' xs'
 where
  tryAgain = pickParagraphs' randomGen' charCount (remainingRetry - 1) pp xs
  (idx, randomGen') = randomR (0, (length xs) - 1) randomGen
  pp'               = T.append pp p
  p                 = xs !! idx
  charCount'        = charCount - T.length p
  xs'               = L.delete p xs
  maxLength         = charCount + paragraphLengthVariation

randomBookParagraphs :: IO [T.Text]
randomBookParagraphs = do
  fetchBookLinksIfNotYet

  -- get first book
  -- TODO: random book?
  firstLink <- head <$> readLocalBookLinks
  putStrLn $ "ebook link: " ++ firstLink

  -- let ps = texts $ tagSelector "p"
  let ps :: Scraper String [String]
      ps = texts $ tagSelector "p"
  Just paragraphs <- scrapeURL firstLink $ ps
  return $ map T.pack paragraphs


randomBookText :: IO T.Text
randomBookText = do
  fetchBookLinksIfNotYet

  -- get first book
  firstLink <- head <$> readLocalBookLinks
  putStrLn $ "ebook link: " ++ firstLink

  bookText <- fetchPlainText $ T.pack firstLink

  return bookText

readLocalBookLinks :: IO [Link]
readLocalBookLinks = do
  Just links <- decodeFileStrict ebookLinksFilename
  return links

fetchBookLinksIfNotYet :: IO ()
fetchBookLinksIfNotYet = do
  hasFile <- doesFileExist ebookLinksFilename
  if hasFile
    then return ()
    else do
      putStrLn "fetch book links..."
      getBookLinks

getBookLinks :: IO ()
getBookLinks = do
  maybeEbookUrls <- scrapBookLinks
  case maybeEbookUrls of
    Nothing   -> putStrLn "scraper parsing error"
    Just urls -> B.writeFile ebookLinksFilename $ encodePretty $ map
      (boolListWebsite ++)
      urls

scrapBookLinks :: IO (Maybe [Link])
scrapBookLinks = scrapeURL bookListUrl bookLinks
 where
  bookLinks :: Scraper String [Link]
  bookLinks = attrs "href" $ "a" @: [match isEbookLink]

isEbookLink :: String -> String -> Bool
isEbookLink attrKey attrVal = attrKey == "href" && isEbookUrl attrVal

isEbookUrl :: String -> Bool
isEbookUrl str = str =~ ebookUrlRegex
