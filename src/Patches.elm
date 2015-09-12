module Patches where

import Matrix exposing (Matrix)
import Color exposing (Color, black, red, green, blue, rgb)
import String
import Debug exposing (log)

import Model exposing (..)
import Utils exposing (..)
import Parser exposing (compileError)
import Stack


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
  in
      case Matrix.get i j model.patches of
        Just v -> 
          let 
            color = Color.toRgb v.pcolor
          in 
            List.foldl Stack.push model <| List.map toString <| List.reverse [color.red, color.green, color.blue]
        Nothing -> incorrectCoords args model

pxcorOf : Argument -> Model -> Model
pxcorOf args model =
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> Stack.push (toString  v.pxcor) model
      Nothing -> incorrectCoords args model

pycorOf : Argument -> Model -> Model
pycorOf args model =
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> Stack.push (toString  v.pycor) model
      Nothing -> incorrectCoords args model

pxycorOf : Argument -> Model -> Model
pxycorOf args model =
  let
    (i, j) = getCoords args model
  in
    case Matrix.get i j model.patches of
      Just v -> List.foldl Stack.push model <| List.reverse <| List.map toString [v.pxcor, v.pycor]
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
  in
    case Matrix.get i j model.patches of 
      Just v -> log "Patch is: " (toString <| v) |> (\_ -> model)
      Nothing -> incorrectCoords args model
      

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