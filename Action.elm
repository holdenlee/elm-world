module Action (..) where

import Keyboard
import Text
import Window
import Dict as D
import Maybe exposing (..)
import List as L
import Set
import Debug exposing (..)
--user-defined modules
import Utilities exposing (..)
import MDict exposing (..)
import Directions exposing (..)
import Actor exposing (..)
import World exposing (..)
import Input exposing (..)

type alias Action = (Actor, World {}) -> (Actor, World {})

getFromArg : (Actor -> Action) -> Action
getFromArg f = (\(ac,w) -> (f ac) (ac,w))

--dirFromInp : (Dir -> Action) -> Action
--dirFromInp f = (\ac w -> (f (getOneKey w.input.keys)) inp ac w)

--facing : Actor -> Int
--facing act = getFirstNonzero [get2 "facing" act.info, get2 "last_move" act.info]

nullAction:Action
nullAction t = t 

--the first actor reacts to the second actor
type alias Reaction = Actor -> Actor -> World {} -> World {}

nullReaction:Reaction
nullReaction _ _ w = w

--creating simple actions
simpleAction : ((Actor, World {}) -> Actor) -> Action
simpleAction f = (\(ac,w) -> 
                               let 
                                   newA = f (ac,w)
                               in      
                                   (newA, updateActor ac newA w))

simpleAction0 : ((Actor, World {}) -> Actor) -> Action
simpleAction0 f = (\(ac,w) -> 
                               let 
                                   newA = f (ac,w)
                               in      
                                   (newA, updateActor0 ac newA w))

setAction : (Actor -> Actor) -> Action
setAction f = simpleAction (\(a,w) -> f a)

dirFromInp : (Dir -> Action) -> Action
dirFromInp f = (check (\(_,w) -> Set.member (getOneKey w.input.keys) dirs)) .& (\(a,w) -> f (getOneKey w.input.keys) (a,w))

--display a message
messageAction : (Actor -> String) -> Action
messageAction f = (\(a,w) -> (a,{w | text <- w.text ++ "\n" ++ (f a)}))

messageAction2 : ((Actor, World {}) -> String) -> Action
messageAction2 f = (\(a,w) -> (a,{w | text <- w.text ++ "\n" ++ (f (a,w))}))

--checking actions

check : ((Actor, World {}) -> Bool) -> Action
check f = simpleAction0 (\(a,w) -> if f (a,w) then asetInt "success" 1 a else asetInt "success" 0 a)

checkActor : (Actor -> Bool) -> Action
checkActor f = check (\(a,_) -> f a)

--movement actions
moveInDir : Int -> Action
moveInDir dir = simpleAction (\(a,w) ->  
                                                  let 
                                                      oldLoc = withDefault (0,0) (a.locs!0)
                                                  in a |> (setLoc (tryMove w oldLoc (dir))) |> (\x -> case (x.locs!0) of
                                                                                                        Just newLoc -> if (if getType a == "player" then (newLoc |> watch "newLoc")==(oldLoc |> watch "oldLoc") else (newLoc == oldLoc))
                                                                                                                       then (asetInt "success" (0 |> (if getType a == "player" then watch "success" else identity)) x)
                                                                                                                       else (asetInt "success" (1 |> (if getType a == "player" then watch "success" else identity)) x)
                                                                                                        Nothing -> (asetInt "success" 0 x)))

moveInDir2 : MoveCondition {} -> Int -> Action
moveInDir2 mc dir = simpleAction (\(a,w) ->  
                                                  let 
                                                      oldLoc = withDefault (0,0) (a.locs!0)
                                                  in a |> (setLoc (tryMove2 mc w oldLoc (dir))) |> (\x -> case (x.locs!0) of
                                                                                                        Just newLoc -> if newLoc==oldLoc 
                                                                                                                       then (asetInt "success" 0 x)
                                                                                                                       else (asetInt "success" 1 x)
                                                                                                        Nothing -> (asetInt "success" 0 x)))

moveIn: Action
moveIn = dirFromInp moveInDir

moveIn2: MoveCondition {} -> Action
moveIn2 mc =  dirFromInp (moveInDir2 mc)

faceDir : Dir -> Action
faceDir dir = setAction (asetInt "face" dir)

face : Action
face = dirFromInp faceDir
--(check (\inp _ _ -> not (getOneKey inp.keys == 0))) .& (\inp -> (faceDir (getOneKey inp.keys)) inp) 
--.& (messageAction (\_ -> "faced"))

velocityDir: Int -> Action
velocityDir dir = setAction (asetInt "v" dir)

velocity : Action
velocity = dirFromInp velocityDir

stop: Action
stop = velocityDir 0

inertia:Action
inertia = (\(a,w) -> (moveInDir (agetInt "v" a)) (a,w)) .| stop

inertia2: MoveCondition {} -> Action
inertia2 mc = (\(a,w) -> (moveInDir2 mc (agetInt "v" a)) (a,w)) .| stop

allActorsSatisfy : (Actor -> Bool) -> MoveCondition {}
allActorsSatisfy f = (\w loc -> inRange w loc && ([] == List.filter (\x -> not (f x)) (getActorsAt loc w)))

--combinators

seqActions: List Action -> Action
seqActions acts = L.foldl (>>) nullAction acts

--Combinators
--assume a revert guarantee if fail
(.&):Action -> Action -> Action
a1 .& a2 = a1 >> (\(a,w) -> if ((agetInt "success" a) == 0) then (a,w) else a2 (a,w))

(.|):Action -> Action -> Action
a1 .| a2 = a1 >> (\(a,w) -> if ((agetInt "success" a) == 0) then a2 (succeed (a,w)) else (a,w))

seqAnd: List Action -> Action
seqAnd = L.foldl (.&) succeed

seqOr : List Action -> Action
seqOr = L.foldl (.|) fail

--returns a success no matter what happens
try:Action -> Action
try a = seqActions [a, succeed]

repeat: Int -> Action -> Action
repeat n a = if n==0 then nullAction else (\(a1,w) -> 
                      if ((agetInt "success" a1) == 0) then (a1,w) |> succeed else (repeat (n-1) a) (a1,w))
--the following is easier to write but less efficient?
--repeat n a = foldl (.&) (repeat n a)

setSuccess: Int -> Action
setSuccess i = simpleAction (\(a,w) -> asetInt "success" i a)

fail: Action
fail = setSuccess 0

succeed: Action
succeed = setSuccess 1

many0: Action -> Action
many0 a = try (\(a1,w) -> if ((agetInt "success" a1) == 0) then (a1,w) else (many0 a) (a (a1,w)))

many: Action -> Action
many a = (a .| fail) .& (many0 a)

getType2: Actor -> String
getType2 a = getStr "type" a.info

--getType is misbehaving.
--getType2: Actor -> String
--getType2 = getType

--make the 2nd actor do something
make: Actor -> Action -> Action
make ac action = (\(orig,w) -> 
                let 
                    t : String
                    t = getType2 ac
                in
                  if (t == "") then fail (orig,w) else 
                      let 
                          (newA, newW) = action (ac, w)
                          i =  agetInt "success" newA
                      in 
                        setSuccess i (orig, newW))
--(added a check to see if it's null)

makeRel : ((Actor, World {}) -> Actor) -> Action -> Action
makeRel f action = (\(orig,w) -> 
                let 
                    t : String
                    t = getType2 (f (orig,w))
                in
                  if (t == "") then fail (orig,w) else 
                      let 
                          (newA, newW) = action (f (orig, w), w)
                          i = agetInt "success" newA
                      in setSuccess i (orig, newW))

doForAll:(b -> Action) -> ((Actor, World {}) -> List b) -> Action
doForAll f g = (\(a,w) -> (seqActions (List.map f (g (a,w)))) (a,w))

--(\(orig,w) -> if (getType2 ((f (orig,w)) == "")) then (orig,w) else action (orig,w))

makeActorIn : Action -> Action
makeActorIn f = makeRel actorIn f

die: Action
die = simpleAction (\_ -> nullActor)

doOrFail: Action -> Action
doOrFail ac = (\(a,w) -> 
              let (a2,w2) = ac (a,w)
              in 
                if ((agetInt "success" a2) == 1)
                   then (a2,w2)
                   else fail (a,w))

showLoc : List (Int,Int) -> String
showLoc locs = case locs!0 of
                 Nothing -> ""
                 Just x -> toString x

{-

(\(a,w) -> 
           let 
               (aa,ww) = a1 (a,w)
           in
             if (agetInt "success" a) == 0 then (a,w) else a2 (
-}
