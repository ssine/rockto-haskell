{-# LANGUAGE OverloadedStrings #-}

module Main where

import UI

import Rockto.Types
import Rockto.Utils (mkInitS)

import qualified System.Random as R (newStdGen)
import qualified Graphics.Vty as V

import Brick.Main (App(..), defaultMain, resizeOrQuit, neverShowCursor)
import Brick.Util (on, fg)
import Brick.AttrMap (attrMap, AttrMap)
import Data.Text.Markup ((@@))


import Control.Monad (void, forever)
import Graphics.Vty as Vty

import Brick (Widget, simpleMain, (<+>), str, withBorderStyle)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

-- main :: IO ()
-- main = simpleMain rocktoApp

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("keyword1",      fg V.magenta)
    , ("keyword2",      V.white `on` V.blue)
    ]

app :: App GSt e ()
app =
    App { appDraw = drawUI
        , appHandleEvent = resizeOrQuit
        , appAttrMap = const theMap
        , appStartEvent = return
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = do
    n <- R.newStdGen
    void $ defaultMain app $ mkInitS n 
