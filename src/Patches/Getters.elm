module Patches.Getters where

import String
import Matrix exposing (Matrix)

import Model exposing (..)
import Utils exposing (..)
import Parser exposing (runtimeError)
import Stack

import Patches.Utils exposing (..)
import Patches.Encoding exposing (..)

patchAt : Argument -> Model -> Model
patchAt args model = 
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> 
        case patchFromString <| patchAsString v of
          Just p -> Stack.push (patchAsString p) model
          Nothing -> runtimeError ["decoding failed!"] model
      Nothing -> incorrectCoords args model

{-|
get the pcolor of a patch at i, j and push it to stack 
-}
pcolorOf : Argument -> Model -> Model
pcolorOf args model = 
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> List.foldl Stack.push model <| patchColorAsString v
      Nothing -> incorrectCoords args model

{-|
get the pxcor of a patch at i, j and push it to stack 
-}
pxcorOf : Argument -> Model -> Model
pxcorOf args model =
  let
    (i, j) = getCoords args model
  in
    if List.length args == 2 then
      case Matrix.get i j model.patches of
        Just v -> Stack.push (toString  v.pxcor) model
        Nothing -> incorrectCoords args model
    else 
      case List.head args of
        Just p -> 
          case patchFromString p of
            Just v -> Stack.push (toString v.pxcor) model
            Nothing -> runtimeError ["Failed to decode patch " ++ p] model
        Nothing -> runtimeError ["Not enough arguments!"] model

{-|
get the pycor of a patch at i, j and push it to stack 
-}
pycorOf : Argument -> Model -> Model
pycorOf args model =
  let
    (i, j) = getCoords args model
  in
    if List.length args == 2 then
      case Matrix.get i j model.patches of
        Just v -> Stack.push (toString  v.pycor) model
        Nothing -> incorrectCoords args model
    else 
      case List.head args of
        Just p -> 
          case patchFromString p of
            Just v -> Stack.push (toString v.pycor) model
            Nothing -> runtimeError ["Failed to decode patch " ++ p] model
        Nothing -> runtimeError ["Not enough arguments!"] model

{-|
get the pxycor of a patch at i, j and push it to stack 
-}
pxycorOf : Argument -> Model -> Model
pxycorOf args model =
  let
    (i, j) = getCoords args model
  in
    if List.length args == 2 then
      case Matrix.get i j model.patches of
        Just v -> List.foldl Stack.push model <| List.reverse <| List.map toString [v.pxcor, v.pycor]
        Nothing -> incorrectCoords args model
    else 
      case List.head args of
        Just p -> 
          case patchFromString p of
            Just v -> List.foldl Stack.push model <| List.reverse <| List.map toString [v.pxcor, v.pycor]
            Nothing -> runtimeError ["Failed to decode patch " ++ p] model
        Nothing -> runtimeError ["Not enough arguments!"] model