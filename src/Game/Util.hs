module Game.Util where

import qualified Data.Text                     as T
import           Brick.Types                    ( Widget )

import           Brick.Widgets.Core             ( withAttr
                                                , emptyWidget
                                                , (<+>)
                                                , (<=>)
                                                , fill
                                                , txtWrap
                                                , padTop
                                                , txt
                                                )
import           Data.Text.Markup               ( (@@) )

lineWidth = 30

-- TODO: create intermediate data structure

computeWidget :: T.Text -> T.Text -> Widget n
computeWidget sampleText userText = foldl (<=>) emptyWidget
  $ zipWith computeShortTextWidget sampleLines userLines
 where
  sampleLines = cutToLines lineWidth sampleText
  userLines   = foldl f [] sampleLines
  f xs sampleLine = xs ++ [newLine]
   where
    newLine =
      T.take (T.length sampleLine) $ T.drop ((T.length . T.concat) xs) userText

computeShortTextWidget :: T.Text -> T.Text -> Widget n
computeShortTextWidget sampleText userText =
  (withAttr "rightText" (txt matchedText))
    <+> (withAttr "wrongText" (txt unmatchedText))
    <+> (withAttr "remainingText" (txt remainingText))
 where
  matchedText = T.pack $ map fst $ takeWhile (\(a, b) -> a == b)
                                             (T.zip sampleText $ userText)
  unmatchedText = T.take (T.length userText - T.length matchedText)
    $ T.drop (T.length matchedText) sampleText
  remainingText =
    T.drop (T.length matchedText) $ T.drop (T.length unmatchedText) sampleText


-- should preserve words
cutToLines :: Int -> T.Text -> [T.Text]
cutToLines n text = foldl f [] $ T.words text
 where
  f [] word = [word]
  f xs word | T.length newLastLine > n = init xs ++ [T.snoc (last xs) ' '] ++ [word]
            | otherwise                = init xs ++ [newLastLine]
   where
    newLastLine = T.append (last xs) word'
    word'       = T.cons ' ' word

-- TODO: calculate WPM