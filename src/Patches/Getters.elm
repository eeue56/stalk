module Patches.Getters where

import String
import Matrix exposing (Matrix)

import Model exposing (..)
import Utils exposing (..)
import Parser.Errors exposing (runtimeError)
import Stack

import Patches.Utils exposing (..)
import Patches.Encoding exposing (..)

patchAt : Argument -> Model -> Model
patchAt args model = 
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> Stack.push (patchAsString v) model
      Nothing -> incorrectCoords args model

builtinPatchGetter : (Patch -> Model -> Model) -> Argument -> Model -> Model 
builtinPatchGetter getter args model = 
  if List.length args == 2 then
    let
      (i, j) = getCoords args model
    in
      case Matrix.get i j model.patches of
        Just v -> getter v model
        Nothing -> incorrectCoords args model
  else 
    case List.head args of
      Just p -> 
        case patchFromString p of
          Just v -> getter v model
          Nothing -> runtimeError ["Failed to decode patch " ++ p] model
      Nothing -> runtimeError ["Not enough arguments!"] model

{-|
get the pcolor of a patch at i, j and push it to stack 
-}
pcolorOf : Argument -> Model -> Model
pcolorOf = 
  builtinPatchGetter (\v model -> List.foldl Stack.push model <| patchColorAsString v)

{-|
get the pxcor of a patch at i, j and push it to stack 
-}
pxcorOf : Argument -> Model -> Model
pxcorOf = 
  builtinPatchGetter (\v -> Stack.push (toString v.pxcor))

{-|
get the pycor of a patch at i, j and push it to stack 
-}
pycorOf : Argument -> Model -> Model
pycorOf =
  builtinPatchGetter (\v -> Stack.push (toString v.pycor))


{-|
get the pxycor of a patch at i, j and push it to stack 
-}
pxycorOf : Argument -> Model -> Model
pxycorOf =
  builtinPatchGetter (\v model -> List.foldl Stack.push model <| List.reverse <| List.map toString [v.pxcor, v.pycor])