module Language.Patches.Setters where

import String
import Debug exposing (log)
import Matrix exposing (Matrix)

import Model exposing (..)
import Utils exposing (..)
import Parser.Errors exposing (runtimeError)

import Language.Stack as Stack

import Language.Patches.Utils exposing (..)
import Language.Patches.Encoding exposing (..)


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
      { model | patches = Matrix.map (\x -> { x | pcolor = newColor }) model.patches }

{-|
  set the pcolor of the patch at i, j
-}
setPcolorOf : Argument -> Model -> Model
setPcolorOf args model =
  if List.length args == 5 then
    let
      (i, j) = getCoords args model
      rgb = List.drop 2 args
      newColor = rgbFromList rgb
      update = (\p -> { p | pcolor = newColor })
    in
      case Matrix.get i j model.patches of
        Just _ -> { model | patches = Matrix.update i j update model.patches }
        Nothing -> incorrectCoords args model
  else
    let
      rgb = List.take 3 args
      newColor = rgbFromList rgb
      update = (\p -> { p | pcolor = newColor })
    in
      case List.head <| List.drop 3 args of
        Just v ->
          case patchFromString v of
            Just p -> { model | patches = Matrix.update p.pxcor p.pycor update model.patches }
            Nothing -> runtimeError ["Failed to decode patch " ++ String.join ", " args] model
        Nothing -> runtimeError ["not enough arguments!"] model
