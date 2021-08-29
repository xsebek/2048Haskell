{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module BotGame (botPlayer2) where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Next,
    Padding (..),
    Widget,
    attrMap,
    attrName,
    continue,
    customMain,
    emptyWidget,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padRight,
    padTop,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Util as U
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.List ()
import Data.Maybe ( Maybe(Just), fromMaybe )
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Graphics.Vty as V
import HumanGame (Name, Tick, drawUI, initGame, move, theMap)
import Linear.V2 (V2 (..))
import Logic (Direction (..), Game (..), Grid, keepTrying)
import Prelude

-- define App
app :: App Game String Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = botEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

--processAction :: Int -> Grid
--processAction i = case i of
--1 ->

botPlayer2 :: [String] -> IO ()
botPlayer2 s = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      --processAction i
      mapM_ (writeBChan chan) ["Up", "Down"]
      -- writeBChan chan "Up"
      threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let mkVty = V.mkVty V.defaultConfig
  vty <- mkVty
  void $ customMain vty mkVty (Just chan) app g

botEvent :: Game -> BrickEvent Name String -> EventM Name (Next Game)
botEvent g = \case
  AppEvent "Up" -> continue $ move Logic.Up g
  AppEvent "Down" -> continue $ move Logic.Down g
  AppEvent "Right" -> continue $ move Logic.Right g
  AppEvent "Left" -> continue $ move Logic.Left g
  _ -> error "Unexpected event"

-- botPlayer :: Int -> IO ()
-- botPlayer i = do
--   chan <- newBChan 10
--   forkIO $ forever $ do
--     --processAction i
--     writeBChan chan "Up"
--     threadDelay 100000 -- decides how fast your game moves
--   g <- initGame
--   void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

-- botEvent :: Game -> BrickEvent Name String -> EventM Name (Next Game)
-- botEvent g (AppEvent s) = continue $ move Logic.Up g
