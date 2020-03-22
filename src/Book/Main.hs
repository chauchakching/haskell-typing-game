module Book.Main where

import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Monoid                    ( mempty )
import qualified Data.ByteString.Char8  as B
import qualified Data.Text                     as T
-- import Data.Text.Encoding (decodeUtf8)
import           Network.HTTP.Req               ( runReq
                                                , defaultHttpConfig
                                                , req
                                                , GET(..)
                                                , NoReqBody(..)
                                                , http
                                                , (/:)
                                                , (/~)
                                                , bsResponse
                                                , responseBody
                                                )
import           Text.HTML.TagSoup

bookListUrl = http "gutenberg.net.au" /: "plusfifty-n-z.html"

-- TODO:
-- parse html

getBookLinks :: IO ()
getBookLinks = runReq defaultHttpConfig $ do
  liftIO $ putStrLn $ "url" ++ (show bookListUrl)
  response <- req GET bookListUrl NoReqBody bsResponse mempty
  liftIO $ writeFile "book-website-html.txt" $ B.unpack $ responseBody response
  -- liftIO $ print $ responseBody response
