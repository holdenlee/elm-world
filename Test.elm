module Test (..) where

import Keyboard
import Text
import Window
import Dict as D
import Maybe exposing (..)
import List
import Set
import Graphics.Element exposing (..)
import Signal exposing (..)
import Debug exposing (..)
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

--playerDraw:DrawActor
--playerDraw = simpleDraw (croppedImage (60,0) 30 30 "/iceblox.gif")
--(image 32 32 "/player.jpg")
ppFace: D.Dict Int Element
ppFace = faceDict (List.map (\t -> croppedImage t 30 30 "iceblox.gif") [(150,0),(60,0),(210,0),(120,30),(60,0)])

ppDraw:DrawActor
ppDraw = faceDraw ppFace

player:Actor
player = nullActor |> setType "player" |> setLoc (0,0)

playerAction:Action
playerAction = fail >> (watch "pp1") >> moveIn >> (watch "pp2")
--fail >> moveInDir upArrow >> watch "pp1"
--works
--fail >> face
--works
--fail >> (watch "pp1") >> moveIn >> (watch "pp2")
--2nd doesn't execute.
--fail >> (watch "pp1")
--ok
--simpleAction (\(a,w) -> asetInt "success" 2 a) >> moveIn
--works
--fail >> moveIn
--fails
-- moveIn >> succeed
--works
--moveIn >> fail
--fails
--moveIn >> (watch "pp1")  >> simpleAction (\(a,w) -> asetInt "success" 0 a)
--fails
-- >> (watch "pp1") >> simpleAction (\(a,w) -> asetInt "success" 0 a) >> (watch "pp2")
--moveIn >> (watch "pp1") >> (messageAction (\a -> toString <| agetInt "success" a)) >> (watch "pp2")
--0, 1, 1
--moveIn >> (watch "pp") 
--success in pp is 1 but alone is 0.
--simpleAction (\(a,w) -> asetInt "success" 0 a) >> (watch "pp") --success is 0
--moveIn >>     
--(messageAction (\a -> toString <| agetInt "success" a)) >> (messageAction (\a -> toString <| agetInt "success" a)) >> (watch "pp")
-- simpleAction (\(a,w) -> asetInt "success" 0 a)

worldStart:Int -> Int -> World2
worldStart x y = (emptyWorld2 x y) 
               |> (addActor player) 
               |> addType "player" playerAction nullReaction ppDraw

worldState : Signal World2
worldState = foldp stepWorld (worldStart 15 15) input

--main : Signal Element
main = Signal.map defaultDisplay worldState
