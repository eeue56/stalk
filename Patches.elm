module Patches where

import Matrix exposing (Matrix)
import Color exposing (Color, black, red, green, blue, rgb)
import String
import Debug exposing (log)

import Model exposing (..)
import Utils exposing (..)


pcolorOf : Argument -> Model -> Model
pcolorOf args model = 
  let
    i = alwaysOkInt <| case List.head args of Just x -> x
    j = alwaysOkInt <| (\y -> case List.head y of Just v -> v) <| case List.tail args of Just x -> x
    colorAsString obj =
      String.join ","
       <| List.map toString [obj.red, obj.green, obj.blue]

    color = 
      case Matrix.get i j model.patches of
        Just v -> colorAsString <| Color.toRgb v.pcolor
        Nothing -> "error"
  in
    { model | stack <- color :: model.stack }

setPcolor : Argument -> Model -> Model
setPcolor color model =
  if List.length color == 1 then
    let
      myHead = case List.head color of 
        Just v -> String.trim v
        Nothing -> "black"
      newColor = if myHead == "red" then red else black
      (width, height) = model.patches.size
    in
      { model | patches <- Matrix.map (\x -> { x | pcolor <- newColor }) model.patches }
  else
    let
      (width, height) = model.patches.size 
      newColor = rgbFromList color
    in
      { model | patches <- Matrix.map (\x -> { x | pcolor <- newColor }) model.patches }

setPcolorOf : Argument -> Model -> Model
setPcolorOf args model =
  let
    i = alwaysOkInt <| case List.head args of Just x -> x
    j = alwaysOkInt <| (\y -> case List.head y of Just v -> v) <| case List.tail args of Just x -> x
    rgb = List.drop 2 args
    newColor = rgbFromList rgb
    update = (\p -> { p | pcolor <- newColor })
  in
    { model | patches <- Matrix.update i j update model.patches }

logPatch : Argument -> Model -> Model
logPatch args model =
  let 
    i = alwaysOkInt <| case List.head args of Just x -> x
    j = alwaysOkInt <| case List.head <| List.reverse args of Just x -> x
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
    board = List.map (\x -> List.map (\y -> defaultPatch x y) [0..width]) [0..height]
  in
    case Matrix.fromList board of
      Just v -> v
      Nothing -> Matrix.empty