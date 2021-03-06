module PushBlocks (..) where

import Keyboard
import Text
import Window
import Dict as D
import Maybe exposing (..)
import List as L
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

player:Actor
player = nullActor |> setType "player" |> setLoc (0,0)

ppAction:Action
ppAction = seqActions [
            messageAction (\a -> showLoc (a.locs)),
            face, 
            moveIn .| makeActorIn (dirFromInp velocityDir)
           ]

ppFace: D.Dict Int Element
ppFace = faceDict (List.map (\t -> croppedImage t 30 30 "iceblox.gif") [(150,0),(60,0),(210,0),(120,30),(60,0)])

ppDraw:DrawActor
ppDraw = faceDraw ppFace

block:Actor
block = nullActor |> setType "block" 
--|> setLoc (5,5)

blockDraw:DrawActor
blockDraw = simpleDraw (croppedImage (0,60) 30 30 "iceblox.gif")

worldStart:Int -> Int -> World2
worldStart x y = (emptyWorld2 x y) 
               |> (addActor player) 
               |> addType "player" ppAction nullReaction ppDraw
               |> (addActorAtLocs block [(5,5),(6,6),(7,7),(5,7)])
               |> addType "block" inertia nullReaction blockDraw
--(addActor block)

worldState = foldp stepWorld (worldStart 15 15) input

main = Signal.map defaultDisplay worldState
