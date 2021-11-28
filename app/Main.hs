{-# LANGUAGE OverloadedStrings #-}

module Main where

import Rockto.Config (callbackDelayTimeMS)
import Rockto.Tick (tick)
import Rockto.Types (Direction (..), GSt (_stable))
import Rockto.Utils (mkInitS)
import UI (drawUI)

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)

import qualified Graphics.Vty as V
import qualified System.Random as R (newStdGen)

import Brick (App (..), AttrMap, BrickEvent (VtyEvent), EventM, Next, attrMap, continue,
              defaultMain, fg, halt, neverShowCursor, on)
import Brick.AttrMap (AttrMap, attrMap)
import Brick.Main (App (..), defaultMain, neverShowCursor)
import Brick.Util (fg, on)


theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [("keyword1", fg V.magenta), ("keyword2", V.white `on` V.blue)]

-- Run delayed callback when `stable` of the returned state is `False`
doTick :: Rockto.Types.Direction -> GSt -> EventM () (Next GSt)
doTick direction st =
  let nst = tick direction st
   in if _stable nst
        then continue nst
        else do
          liftIO (threadDelay callbackDelayTimeMS)
          doTick DNull nst

doTickNoCallback :: Rockto.Types.Direction -> GSt -> EventM () (Next GSt)
doTickNoCallback direction st = continue $ tick direction st

handleEvent :: GSt -> BrickEvent () e -> EventM () (Next GSt)
handleEvent st (VtyEvent (V.EvKey key [])) =
  case key of
    V.KEsc      -> halt st
    V.KChar 'q' -> halt st
    V.KUp       -> doTickNoCallback DUp st
    V.KDown     -> doTickNoCallback DDown st
    V.KLeft     -> doTickNoCallback DLeft st
    V.KRight    -> doTickNoCallback DRight st
    _           -> continue st
handleEvent st _ = continue st

app :: App GSt e ()
app =
  App
    { appDraw = drawUI
    , appHandleEvent = handleEvent
    , appAttrMap = const theMap
    , appStartEvent = return
    , appChooseCursor = neverShowCursor
    }

main :: IO ()
main = do
  n <- R.newStdGen
  void $ defaultMain app $ mkInitS n
