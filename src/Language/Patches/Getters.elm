module Language.Patches.Getters where

import String
import Matrix exposing (Matrix)
import Matrix.Extra exposing (neighbours)
import Debug exposing (log)

import Model exposing (..)
import Utils exposing (..)
import Parser.Errors exposing (runtimeError)
import Language.Stack as Stack

import Language.Patches.Utils exposing (..)
import Language.Patches.Encoding exposing (..)

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
  -- if there's two args on the stack, then assume it's patch coordinates
  -- and use those for getting the patch 
  case args of
    x::y::[] ->
      let
        (i, j) = getCoords args model
      in
        case Matrix.get i j model.patches of
          Just v -> getter v model
          Nothing -> incorrectCoords args model
    x::[] -> 
      case List.head args of
        Just p -> 
          case patchFromString p of
            Just v -> getter v model
            Nothing -> runtimeError ["Failed to decode patch " ++ p] model
        Nothing -> runtimeError ["Not enough arguments!"] model
    _ -> 
      runtimeError ["Not sure what to do with arguments given"] model

{-|
get the pcolor of a patch at i, j and push it to stack 
-}
pcolorOf : Argument -> Model -> Model
pcolorOf = 
  builtinPatchGetter (\v model -> Stack.pushManyStrings (List.reverse <| patchColorAsString v) model)

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
  builtinPatchGetter (\v model -> Stack.pushManyStrings (List.reverse <| List.map toString [v.pxcor, v.pycor]) model)

neighboursOf : Argument -> Model -> Model
neighboursOf =
  builtinPatchGetter (\v model -> Stack.pushManyStrings (List.map (patchAsString) <| neighbours v.pxcor v.pycor model.patches) model)