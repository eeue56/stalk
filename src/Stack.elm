module Stack where

import Model exposing (..)
import Parser exposing (..)
import String


emptyStack : Argument -> Model -> Model
emptyStack _ model = 
  { model | stack <- [] }

popOffStack : Argument -> Model -> Model
popOffStack _ model =
  model

pushToStack : Argument -> Model -> Model
pushToStack args model =
  { model | stack <- args ++ model.stack}

repeatTopOfStack : Argument -> Model -> Model
repeatTopOfStack args model =
  case List.head args of 
    Just v -> 
      case String.toInt v of
        Ok x -> case List.head model.stack of
          Just n -> List.foldl push model <| List.repeat x n
          Nothing -> compileError ["not enough items on stack for repeat!"] model
        Err m -> compileError [m ++ " invalid arguments: " ++ String.join ", " args] model
    Nothing -> compileError ["not enough arguments: " ++ String.join ", " args] model


push : String -> Model -> Model
push item model =
  { model | stack <- item :: model.stack }

swap : Model -> Model
swap model = 
  let
    items = List.take 2 model.stack
    notEnough = compileError ["not enough items on stack for swap!"] model
  in
    if List.length items < 2 then
      notEnough
    else
      case List.head items of
        Just x -> case List.head <| List.drop 1 items of
          Just y -> { model | stack <- [y, x] ++ List.drop 2 model.stack }
          Nothing -> notEnough
        Nothing -> notEnough