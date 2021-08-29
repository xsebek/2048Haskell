{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Logic (mainLogic, keepTrying)
import HumanGame (humanPlayer)
import BotGame (botPlayer2)

import Brick
  (App(..), AttrMap, AttrName, BrickEvent(..), EventM, Next, Padding(..),
  Widget, attrMap, attrName, continue, customMain, defaultMain, emptyWidget,
  fg, forceAttrMap, hBox, hLimit, halt, neverShowCursor, on, padAll, padLeft,
  padRight, padTop, simpleMain, str, vBox, vLimit, withAttr, withBorderStyle,
  (<+>))
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

drawInfo :: Widget ()
drawInfo = withBorderStyle BS.unicodeBold
  $ C.hCenter
  $ hLimit 80
  $ vLimit 400
  $ B.borderWithLabel (str "Select mode (type key)")
  $ vBox $ map (uncurry drawKey) processList
    where
      drawKey act key = padRight Max (padLeft (Pad 1) $ str act)
                        <+> padLeft Max (padRight (Pad 1) $ str key)


data Process = Human | UpBot | RandBot | CarlBot deriving (Eq, Ord, Enum)

processHelp :: Process -> String
processHelp = \case
  Human -> "Human Player (YOU!)"
  UpBot -> "Up Bot (Always moves up)"
  RandBot -> "Random Bot"
  CarlBot -> "Monte Carlo Bot"

processKey :: Process -> Char
processKey = \case
  Human -> 'h'
  UpBot -> 'u'
  RandBot -> 'r'
  CarlBot -> 'm'

processList :: [(String, String)]
processList = map (\p -> ([processKey p], processHelp p)) [Human, CarlBot]

mayProcess :: Char -> Maybe Process
mayProcess = \case
  'h' -> Just Human
  'u' -> Just UpBot
  'r' -> Just RandBot
  'm' -> Just CarlBot
  _ -> Nothing

process :: Process -> IO ()
process = \case
  Human -> humanPlayer
  CarlBot -> botPlayer2 $ keepTrying
    [ [Just 2, Just 2, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing]
    ]
    (4,4,4)
  _ -> putStrLn "Sorry this is currently not supported"

homeScreen :: App (Maybe Process) e ()
homeScreen = App
      { appDraw = const [drawInfo]
      , appChooseCursor = \_ _-> Nothing
      , appHandleEvent = const endOnKey
      , appStartEvent = pure
      , appAttrMap = const (forceAttrMap V.defAttr)
      }
  where
    endOnKey :: BrickEvent n e -> EventM n (Next (Maybe Process))
    endOnKey = maybe (continue Nothing) (halt . Just)  . handleSwitch

handleSwitch :: BrickEvent n e -> Maybe Process
handleSwitch (VtyEvent (V.EvKey (V.KChar c) _)) = mayProcess c
handleSwitch _ = Nothing

main :: IO ()
main = do
  Just p <- defaultMain homeScreen Nothing
  process p
