module Patches where

import Matrix exposing (Matrix)
import Color exposing (Color, black, red, green, blue, rgb)
import String
import Debug exposing (log)

import Model exposing (..)
import Utils exposing (..)
import Parser exposing (runtimeError)
import Stack


incorrectCoords : Argument -> Model -> Model
incorrectCoords args model =
  runtimeError ["invalid coords: " ++ String.join ", " args] model

{-|
take the first two items from Arguments as i and j
if they fail to parse to ints or not enough, then return width + 1,
causing Matrix.get to return Nothing and allowing errors down the chain
-}
getCoords : Argument -> Model -> (Int, Int)
getCoords args model = 
  let
    (width, height) = model.patches.size
    i = 
      case List.head args of 
        Just x -> 
          case String.toInt x of
            Ok v -> v
            Err _ -> width + 1
        Nothing -> width + 1
    j = 
      case List.head (List.drop 1 args) of 
        Just x -> 
          case String.toInt x of
            Ok v -> v
            Err _ -> height + 1
        Nothing -> height + 1
  in 
    (i, j)

patchColorAsString : Patch -> List String
patchColorAsString p =
  let
    color = Color.toRgb p.pcolor
  in
    List.map toString <| List.reverse [color.red, color.green, color.blue]

patchAsString : Patch -> String
patchAsString patch =
  String.join "~" [
    "pcolor = " ++ (String.join " " <| patchColorAsString patch),
    "pxcor = " ++ (toString patch.pxcor),
    "pycor = " ++ (toString patch.pycor)
  ]

patchAt : Argument -> Model -> Model
patchAt args model = 
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> Stack.push (patchAsString v) model
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
    case Matrix.get i j model.patches of
      Just v -> Stack.push (toString  v.pxcor) model
      Nothing -> incorrectCoords args model

{-|
get the pycor of a patch at i, j and push it to stack 
-}
pycorOf : Argument -> Model -> Model
pycorOf args model =
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> Stack.push (toString  v.pycor) model
      Nothing -> incorrectCoords args model

{-|
get the pxycor of a patch at i, j and push it to stack 
-}
pxycorOf : Argument -> Model -> Model
pxycorOf args model =
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> List.foldl Stack.push model <| List.reverse <| List.map toString [v.pxcor, v.pycor]
      Nothing -> incorrectCoords args model

{-|
set the pcolor of all patches
-}
setPcolor : Argument -> Model -> Model
setPcolor color model =
  if List.length color < 3 then 
    runtimeError ["Not enough arguments for pcolor!"] model
  else
    let
      (width, height) = model.patches.size 
      newColor = rgbFromList color
    in
      { model | patches <- Matrix.map (\x -> { x | pcolor <- newColor }) model.patches }

{-|
  set the pcolor of the patch at i, j
-}
setPcolorOf : Argument -> Model -> Model
setPcolorOf args model =
  let
    (i, j) = getCoords args model
    rgb = List.drop 2 args
    newColor = rgbFromList rgb
    update = (\p -> { p | pcolor <- newColor })
  in
    case Matrix.get i j model.patches of
      Just _ -> { model | patches <- Matrix.update i j update model.patches }
      Nothing -> incorrectCoords args model

{-|
log a patch to console.log
-}
logPatch : Argument -> Model -> Model
logPatch args model =
  let 
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of 
      Just v -> log "Patch is: " (toString <| v) |> (\_ -> model)
      Nothing -> incorrectCoords args model
      

{-|
reset patches to default
-}
clearPatches : Model -> Model 
clearPatches model =
  let
    (width, height) = model.patches.size
    board = List.map (\x -> List.map (\y -> defaultPatch x y) [0..width]) [0..height]
  in
    { model | patches <- 
      case Matrix.fromList board of
        Just v -> v
        Nothing -> Matrix.empty
        }

defaultPatch x y = { pcolor = black, pxcor = x, pycor = y }
redPatch x y = { pcolor = red, pxcor = x, pycor = y }
colorPatch color x y = { pcolor = color, pxcor = x, pycor = y } 

defaultPatches width height = 
  let
    board = List.map (\y -> List.map (\x -> defaultPatch x y) [0..width]) [0..height]
  in
    case Matrix.fromList board of
      Just v -> v
      Nothing -> Matrix.empty