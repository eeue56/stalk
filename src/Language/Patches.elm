module Language.Patches where

import Matrix exposing (Matrix)

import Color exposing (Color, black, red, green, blue, rgb)
import String
import Debug exposing (log)

import Model exposing (..)
import Utils exposing (..)
import Parser.Errors exposing (runtimeError)

import Language.Stack as Stack
import Language.Patches.Utils exposing (..)
import Language.Patches.Encoding exposing (..)

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
    board = defaultPatches width height
  in
    { model | patches = board }

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
