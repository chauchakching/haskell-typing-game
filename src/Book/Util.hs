module Book.Util where

import qualified  Data.List as L
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Monoid                    ( mempty )
import qualified Data.ByteString.Char8  as B
import qualified Data.Text                     as T
import           Text.Regex.TDFA                ( (=~) )
import           System.Random                  ( StdGen
                                                , getStdGen
                                                , newStdGen
                                                )
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
                                                , useURI
                                                )
import Text.URI (mkURI)

pullStdGen :: IO StdGen
pullStdGen = do 
  g <- getStdGen
  newStdGen 
  return g
  

fetchPlainText :: T.Text -> IO T.Text
fetchPlainText str = do 
  Just httpOrHttpsUrl <- useURI <$> mkURI str
  response <- runReq defaultHttpConfig $ case httpOrHttpsUrl of 
      Left (httpUrl, options) -> req GET httpUrl NoReqBody bsResponse mempty
      Right (httpsUrl, options) -> req GET httpsUrl NoReqBody bsResponse mempty
  return $ T.pack $ B.unpack $ responseBody response

isSentenceTerminator :: Char -> Bool
isSentenceTerminator char = elem char ("[?!.]" :: String) 

takeFirstSentence :: T.Text -> T.Text
takeFirstSentence txt = case maybeFirstBreaker of
    Nothing -> ""
    Just idx -> T.take (idx + 1) txt
  where maybeFirstBreaker = T.findIndex isSentenceTerminator txt