{-# LANGUAGE OverloadedStrings #-}

module Main where

import UI (drawUI)

import Rockto.Types
import Rockto.Utils (mkInitS)
import Rockto.Tick (tick)


import qualified Graphics.Vty as V
import qualified System.Random as R (newStdGen)

import Brick
import Brick.AttrMap (AttrMap, attrMap)
import Brick.Main (App (..), defaultMain, neverShowCursor)
import Brick.Util (fg, on)

import Control.Monad (forever, void)


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("keyword1",      fg V.magenta)
    , ("keyword2",      V.white `on` V.blue)
    ]

handleEvent :: GSt -> BrickEvent () e -> EventM () (Next GSt)
handleEvent st (VtyEvent (V.EvKey key [])) =
    case key of
        V.KEsc      -> halt st
        V.KChar 'q' -> halt st
        V.KUp       -> continue $ tick DUp st
        V.KDown     -> continue $ tick DDown st
        V.KLeft     -> continue $ tick DLeft st
        V.KRight    -> continue $ tick DRight st
        _           -> continue st
handleEvent st _ = continue st

app :: App GSt e ()
app =
    App { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appAttrMap = const theMap
        , appStartEvent = return
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = do
    n <- R.newStdGen
    void $ defaultMain app $ mkInitS n
