module Move (..) where

import Keyboard
import Text
import Window
import Dict as D
import Maybe exposing (..)
import List
import Set
import Graphics.Element exposing (..)
import Signal exposing (..)
--user-defined modules
import Utilities exposing (..)
import MDict exposing (..)
import Directions exposing (..)
import Actor exposing (..)
import World exposing (..)
import Action exposing (..)
import Input exposing (..)
import World2 exposing (..)
import WorldView exposing (..)

playerDraw:DrawActor
playerDraw = simpleDraw (croppedImage (60,0) 30 30 "/iceblox.gif")
--(image 32 32 "/player.jpg")

player:Actor
player = nullActor |> setType "player" |> setLoc (0,0)

playerAction:Action
playerAction = seqActions [face, moveIn, messageAction (\a -> toString (withDefault (0,0) (a.locs!0)))]

worldStart:Int -> Int -> World2
worldStart x y = (emptyWorld2 x y) 
               |> (addActor player) 
               |> addType "player" playerAction nullReaction playerDraw

worldState : Signal World2
worldState = foldp stepWorld (worldStart 15 15) input

--main : Signal Element
main = Signal.map defaultDisplay worldState
