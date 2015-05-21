module Input (..) where

import Keyboard
import Text
import Window
import Dict as D
import Maybe exposing (..)
import List
import Set as S
--user-defined modules
import Utilities exposing (..)
import Time exposing (..)

type alias Input = {keys:S.Set Keyboard.KeyCode, delta:Time} 

nullInput:Input
nullInput = {keys = S.empty, delta = 0}

getOneKey:S.Set Keyboard.KeyCode -> Keyboard.KeyCode
getOneKey s  = case (S.toList s)!0 of
                 Just x -> x
                 Nothing -> 0

