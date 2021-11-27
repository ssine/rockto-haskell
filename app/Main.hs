module Main where

import UI

import Control.Monad (void, forever)
import Graphics.Vty as Vty

import Brick (Widget, simpleMain, (<+>), str, withBorderStyle)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

main :: IO ()
main = simpleMain rocktoApp
