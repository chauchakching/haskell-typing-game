module Game.UI where

import           Text.Printf                    ( printf )
import           Data.Foldable                  ( for_ )
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Monad                  ( void
                                                , forever
                                                , when
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Graphics.Vty                   ( white
                                                , red
                                                , green
                                                , blue
                                                , black
                                                , yellow
                                                , rgbColor
                                                , mkVty
                                                , defaultConfig
                                                )
import qualified Graphics.Vty.Input.Events     as VE
import qualified Graphics.Vty.Attributes       as VAttr
import           Control.Lens
import           Control.Lens.TH                ( makeLenses )
import qualified Brick                         as B
import           Brick.Util                     ( fg
                                                , bg
                                                , on
                                                )
import           Brick.Widgets.Core             ( withAttr
                                                , (<+>)
                                                , (<=>)
                                                , fill
                                                , txtWrap
                                                , padTop
                                                , padAll
                                                , txt
                                                , str
                                                , emptyWidget
                                                )
import qualified Brick.Widgets.Edit            as BEdit
import qualified Brick.Widgets.Dialog          as D
import qualified Brick.Widgets.Center          as C
import qualified Brick.AttrMap                 as A
import           Brick.Main                     ( App(..)
                                                , showFirstCursor
                                                )

import           Brick.Types                    ( Widget
                                                , BrickEvent(..)
                                                , EventM
                                                , Next
                                                , Location
                                                , Padding(..)
                                                )
import           Brick.BChan                    ( BChan
                                                , newBChan
                                                , writeBChan
                                                )
import           Constant                       ( text )
import           Game.Util                      ( computeWidget )

grey = rgbColor 130 130 130

data WidgetName = TypingArea
  deriving (Show, Eq, Ord)

data CounterEvent = NthGame Int

data EndingDialogAction = NewGame | Quit
  deriving Show

data St = St
  { _stEditor :: BEdit.Editor T.Text WidgetName
  , _stEndingDialog :: D.Dialog EndingDialogAction
  , _stCounter :: Int
  , _stTimer :: Int
  , _stGameEnded :: Bool
  , _stBChan :: BChan CounterEvent
  , _stPlayCount :: Int
  }

makeLenses ''St

initialEditor :: BEdit.Editor T.Text WidgetName
initialEditor = BEdit.editor TypingArea Nothing "" -- initial content

initialEndingDialog :: D.Dialog EndingDialogAction
initialEndingDialog = D.dialog title (Just (initialOption, options)) maxWidth
 where
  title         = Nothing
  initialOption = 0
  options       = [("New game", NewGame), ("Quit", Quit)]
  maxWidth      = 50

drawUI :: St -> [Widget WidgetName]
drawUI s =
  [ if _stGameEnded s then endingDialogWidget else emptyWidget
  , timerWidget
    <=> wpmWidget
    <=> padTop (Pad 1) (computeWidget text $ getEditorText $ _stEditor s)
    <=> padTop
          (Pad 1)
          (BEdit.renderEditor (B.txt . T.intercalate "\n") True (_stEditor s))
  , fill '.'
  ]
 where
  timerWidget = txt $ T.append "Time: " $ T.pack $ show $ _stCounter s
  wpmWidget =
    str $ (++) "WPM: " $ if wpm s > 0 then printf "%.f" $ wpm s else "-"
  endingDialogWidget =
    D.renderDialog (_stEndingDialog s) $ C.hCenter $ padAll 1 $ txt
      "Game Completed!"

wpm :: St -> Float
wpm s = if counter > 0 then typedCharCount / 5 / counter * 60 else 0
 where
  typedCharCount = fromIntegral $ T.length $ getEditorText $ _stEditor s
  counter        = fromIntegral $ _stCounter s

appEvent
  :: St -> BrickEvent WidgetName CounterEvent -> EventM WidgetName (Next St)
appEvent s e = case e of
  B.VtyEvent ve@(VE.EvKey k ms) -> case (k, ms) of
    (VE.KEsc, []) -> B.halt s
    _             -> if _stGameEnded s
      then do
        newDialog <- D.handleDialogEvent ve $ _stEndingDialog s
        let s' = s & stEndingDialog .~ newDialog
        case k of
          VE.KEnter -> case D.dialogSelection newDialog of
            -- TODO: create new game counter
            Just NewGame -> do
              liftIO $ prepareNextGameTick nGame chan
              B.continue $ initialState chan nGame
                where nGame = _stPlayCount s + 1
                      chan = _stBChan s

            Just Quit    -> B.halt s'
            _            -> B.continue s'
          _ -> B.continue s'
      else do
        -- let editor handle all ot her events
        newEditor <- BEdit.handleEditorEvent ve $ _stEditor s
        let useText   = getEditorText newEditor
        let gameEnded = useText == text
        let newSt = s & stEditor .~ newEditor & stGameEnded .~ gameEnded
        B.continue $ newSt

  -- Stupid global counter
  -- Only work for the first game...
  AppEvent (NthGame n) -> 
    if _stGameEnded s 
      then
        B.continue s
      else do
        liftIO $ when (n == (_stPlayCount s)) $ prepareNextGameTick (_stPlayCount s) (_stBChan s) 
        B.continue $ s & stCounter .~ (_stCounter s + 1)

  _ -> B.continue s

prepareNextGameTick :: Int -> BChan CounterEvent -> IO ()
prepareNextGameTick nGame bChan = void $ forkIO $ do
  threadDelay $ 1*1000*1000
  writeBChan bChan $ NthGame nGame

getEditorText :: BEdit.Editor T.Text n -> T.Text
getEditorText = T.intercalate " " . BEdit.getEditContents

initialState :: BChan CounterEvent -> Int -> St
initialState bChan playCount = St { _stEditor       = initialEditor
                  , _stEndingDialog = initialEndingDialog
                  , _stCounter      = 0
                  , _stTimer        = 60
                  , _stGameEnded    = False
                  , _stBChan        = bChan
                  , _stPlayCount = playCount
                  }

theMap :: A.AttrMap
theMap = A.attrMap
  VAttr.defAttr
  [ (D.dialogAttr        , white `on` blue)
  , (D.buttonAttr        , black `on` white)
  , (D.buttonSelectedAttr, bg yellow)
  , ("cursor", bg white `VAttr.withStyle` VAttr.blink)
  , ("wrongText"         , white `on` red)
  , ("rightText"         , fg green)
  , ("remainingText"     , fg grey)
  ]

theApp = App { appDraw         = drawUI
             , appChooseCursor = showFirstCursor
             , appHandleEvent  = appEvent
             , appStartEvent   = return
             , appAttrMap      = const theMap
             }

play :: IO ()
play = do
  eventChan <- newBChan 10
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty

  -- try counter
  _          <- prepareNextGameTick 1 eventChan

  _ <- B.customMain initialVty buildVty (Just eventChan) theApp $ initialState eventChan 1
  putStrLn "GG EZ"
