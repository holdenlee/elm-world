module WorldView (..) where

import Keyboard
import Text
import Window
import Dict as D
import Maybe exposing (..)
import List as L
import Set
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
--user-defined modules
import Utilities exposing (..)
import MDict exposing (..)
import Directions exposing (..)
import Actor exposing (..)
import World exposing (..)
import Action exposing (..)
import Input exposing (..)
import World2 exposing (..)

--ht:Int
--ht = 15
--
--wd:Int
--wd = 15
--
--pixels:Int
--pixels = 30
--
--sidebarL : Int
--sidebarL = 50
--
--
--hp:Int
--hp = (ht*pixels)
--
--wp:Int
--wp = (wd*pixels)

--playerDraw

--the window to display
defaultViewCoord:World2 -> (Int,Int,Int,Int)
defaultViewCoord w = (0,0,14,14)

--given the width w, height h, (x,y) where the elements should be situated, the elements, creates an element combining all of these.
collageAbs : Int -> Int -> List (Int,Int) -> List Element -> Element
collageAbs w h tuples elems = 
    collage w h (zipMap (\(x,y) -> \elem -> move ( (-(toFloat w)/2 + (toFloat (widthOf elem))/2 + toFloat x),  (-(toFloat h)/2 + (toFloat (heightOf elem))/2 + toFloat y)) (toForm elem)) tuples elems)

display : Int -> Int -> Int -> Int -> (World2 -> (Int, Int, Int, Int)) -> World2 -> Element
display width height pixs sidebarL f w =
   let 
       (x0,y0,x1,y1) = f w
       coordList = listprod (range x0 x1) (range y0 y1)
       -- a list of (x,y),actor@(x,y)
       drawList:List ((Int,Int),Int)
       drawList = L.concat (List.map (\(x,y) -> List.map (\a -> ((x,y), a)) (Set.toList (mget (x,y) w.alocs))) coordList)
       -- the coordinates to draw
       drawCoords = List.map (\(x,y) -> pixs `smult` x) drawList
       drawPics = List.map (\(t,a) -> drawActor (getActor a w) t w) drawList
       totalH = height*pixs
       totalW = width*pixs
   in
       collageAbs (totalW+sidebarL) totalH ((0,0)::(totalW,totalH-300)::drawCoords)
            ((image totalW totalH "bg.gif")::(show w.text)::drawPics)

defaultDisplay : World2 -> Element
defaultDisplay = display 15 15 30 200 defaultViewCoord