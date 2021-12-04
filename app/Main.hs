{-# LANGUAGE OverloadedStrings #-}

module Main where

import Rockto.Config (firstRound, maxRound, tickDelayTimeNS)
import Rockto.Resource (loadGame)
import Rockto.Tick (tick)
import Rockto.Types (Direction (..), GSt (..))
import Rockto.Utils (initSt)
import UI (drawUI)

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)

import qualified System.Random as R (newStdGen)

import qualified Graphics.Vty as V

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, Next)
import Brick.AttrMap (attrMap)
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (App (..), continue, customMain, halt, neverShowCursor)


--------------------------------------------------------------------------------
-- | Game Control Flow
--------------------------------------------------------------------------------

startGame :: Int -> GSt -> EventM () (Next GSt)
startGame round st = do
  game <- liftIO $ loadGame round
  continue $ initSt game (_seed st)

nextRound :: GSt -> EventM () (Next GSt)
nextRound st =
  if round == maxRound
    then continue st
    else startGame (round + 1) st
  where round = _round st

restart :: GSt -> EventM () (Next GSt)
restart st = startGame (_round st) st

newGame :: GSt -> EventM () (Next GSt)
newGame = startGame firstRound

--------------------------------------------------------------------------------
-- | Event Handler
--------------------------------------------------------------------------------

data EvTick = EvTick

handleEvent :: GSt -> BrickEvent () EvTick -> EventM () (Next GSt)
-- Animation
handleEvent st@GSt{_stable=False} (AppEvent EvTick)   = continue $ tick DNull st
-- Next round
handleEvent st@GSt{_finish=True} (AppEvent EvTick)    = nextRound st
handleEvent st (AppEvent EvTick)                      = continue st
-- Quit
handleEvent st (VtyEvent (V.EvKey V.KEsc []))         = halt st
handleEvent st (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt st
-- Reset
handleEvent st (VtyEvent (V.EvKey (V.KChar '1') []))  = restart st
handleEvent st (VtyEvent (V.EvKey (V.KChar 'r') []))  = restart st
handleEvent st (VtyEvent (V.EvKey (V.KChar '2') []))  = newGame st
handleEvent st (VtyEvent (V.EvKey (V.KChar 's') []))  = newGame st
-- Next round
handleEvent st@GSt{_finish=True} _                    = nextRound st
-- Skip while unstable
handleEvent st@GSt{_stable=False} _                   = continue st
-- Stop response when dead
handleEvent st@GSt{_dead=True} _                      = continue st
-- User input
handleEvent st (VtyEvent (V.EvKey key []))            =
  case key of
    V.KUp    -> continue $ tick DUp st
    V.KDown  -> continue $ tick DDown st
    V.KLeft  -> continue $ tick DLeft st
    V.KRight -> continue $ tick DRight st
    _        -> continue st
handleEvent st _                                      = continue st

--------------------------------------------------------------------------------
-- | Application and Entry Point
--------------------------------------------------------------------------------

app :: App GSt EvTick ()
app =
  App
    { appDraw = drawUI
    , appHandleEvent = handleEvent
    , appAttrMap = const $ attrMap V.defAttr []
    , appStartEvent = return
    , appChooseCursor = neverShowCursor
    }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan EvTick
    threadDelay tickDelayTimeNS
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  n <- R.newStdGen
  game <- loadGame firstRound
  void $ customMain initialVty buildVty (Just chan) app $ initSt game n
