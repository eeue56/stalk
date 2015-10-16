module Language.Turtles.Setters where

import Model exposing (..)

import Parser.Errors exposing (runtimeError)

import Color exposing (orange)

import String exposing (toInt)

spawn : Argument -> Model -> Model
spawn args model =
  case args of 
    x::y::_ -> 
      case (toInt x, toInt y) of
        (Ok x', Ok y') ->
          let
            newTurtle = { xcor = x', ycor = y', color = orange }
          in
            { model |  turtles <- newTurtle :: model.turtles }
        _ -> runtimeError ["Failed to convert spawn arguments!"] model
    _ -> runtimeError ["Not enough arguments for spawn!"] model