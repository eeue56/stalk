module Language.Turtles.Getters where 


import Model exposing (..)
import Language.Patches.Encoding exposing (patchFromString)

import Language.Stack as Stack

import Parser.Errors exposing (runtimeError)

import String exposing (toInt)


turtleAt : Int -> Int -> List Turtle -> Maybe Turtle
turtleAt x y turtles =
  case List.filter (\turtle -> turtle.xcor == x && turtle.ycor == y) turtles of
    [] -> Nothing
    turtle::_ -> Just turtle 

turtleOn : Argument -> Model -> Model
turtleOn args model =
  case args of 
    [] -> runtimeError ["Not enough arguments!"] model
    x::y::[] -> 
      case (toInt x, toInt y) of
        (Ok x', Ok y') -> 
          case turtleAt x' y' model.turtles of 
            Nothing -> runtimeError ["No such turtle!"] model
            Just v -> Stack.pushItem v model
        _ -> runtimeError ["Failed to convert arguments to int!"] model
    x::_ -> 
      case patchFromString x of
        Nothing -> runtimeError ["Failed to decode patch!"] model
        Just v -> 
          case turtleAt v.pxcor v.pycor model.turtles of 
            Nothing -> runtimeError ["No such turtle!"] model
            Just v -> Stack.pushItem v model
