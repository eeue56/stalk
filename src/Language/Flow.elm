module Language.Flow where

import Model exposing (..)
import Parser.Errors exposing (runtimeError)

import String
import Dict

import Debug exposing (log)

addLabel : Argument -> Int -> Model -> Model
addLabel args lineNumber model =
  case args of
    [] -> model
    x::_ -> 
      let
        name = String.trim x
      in 
        { model | labels <- log "labels " <| Dict.insert name lineNumber model.labels } 

jumpTo : Argument -> Model -> Model 
jumpTo args model =
  case args of 
    [] -> model
    x::xs ->
      let 
        name = String.trim x
      in 
        case Dict.get name model.labels of
          Just v -> model
          Nothing -> runtimeError ["Label not found: " ++ name] model