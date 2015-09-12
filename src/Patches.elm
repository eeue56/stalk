module Patches where

import Matrix exposing (Matrix)
import Color exposing (Color, black, red, green, blue, rgb)
import String
import Debug exposing (log)

import Model exposing (..)
import Utils exposing (..)
import Parser exposing (compileError)


incorrectCoords : Argument -> Model -> Model
incorrectCoords args model =
  compileError ["incorrect coords: " ++ String.join ", " args] model

getCoords : Argument -> Model -> (Int, Int)
getCoords args model = 
  let
    (width, height) = model.patches.size
    i = 
      case List.head args of 
        Just x -> alwaysOkInt <| x
        Nothing -> width + 1
    j = 
      case List.head (List.drop 1 args) of 
        Just v -> alwaysOkInt <| v
        Nothing -> height + 1
  in 
    (i, j)

pcolorOf : Argument -> Model -> Model
pcolorOf args model = 
  let
    (i, j) = getCoords args model
    colorAsString obj =
      String.join ","
       <| List.map toString [obj.red, obj.green, obj.blue]
  in
      case Matrix.get i j model.patches of
        Just v -> 
          let 
            color = colorAsString <| Color.toRgb v.pcolor
          in 
            { model | stack <- color :: model.stack }     
        Nothing -> incorrectCoords args model

pxcorOf : Argument -> Model -> Model
pxcorOf args model =
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> { model | stack <- (toString v.pxcor) :: model.stack }
      Nothing -> incorrectCoords args model

pycorOf : Argument -> Model -> Model
pycorOf args model =
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> { model | stack <- (toString v.pycor) :: model.stack }
      Nothing -> incorrectCoords args model

pxycorOf : Argument -> Model -> Model
pxycorOf args model =
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> { model | stack <- (String.join "," [toString v.pxcor, toString v.pycor]) :: model.stack }
      Nothing -> incorrectCoords args model

setPcolor : Argument -> Model -> Model
setPcolor color model =
  if List.length color == 1 then
    let
      (width, height) = model.patches.size
    in
      case List.head color of
        Just v -> 
          let
            newColor = if String.trim v == "red" then red else black
          in
            { model | patches <- Matrix.map (\x -> { x | pcolor <- newColor }) model.patches }
        Nothing -> compileError ["Not enough arguments for pcolor!"] model
  else
    let
      (width, height) = model.patches.size 
      newColor = rgbFromList color
    in
      { model | patches <- Matrix.map (\x -> { x | pcolor <- newColor }) model.patches }

setPcolorOf : Argument -> Model -> Model
setPcolorOf args model =
  let
    (i, j) = getCoords args model
    rgb = List.drop 2 args
    newColor = rgbFromList rgb
    update = (\p -> { p | pcolor <- newColor })
  in
    { model | patches <- Matrix.update i j update model.patches }

logPatch : Argument -> Model -> Model
logPatch args model =
  let 
    (i, j) = getCoords args model
    patch = case Matrix.get i j model.patches of 
      Just v -> log "Patch is: " v
      Nothing -> log "erm" (case Matrix.get 0 0 model.patches of Just v -> v)
  in
    (log <| toString <| patch) 
      |> (\_ -> model)

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