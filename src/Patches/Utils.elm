module Patches.Utils where

import String

import Model exposing (..)
import Utils exposing (..)
import Parser exposing (runtimeError)

incorrectCoords : Argument -> Model -> Model
incorrectCoords args model =
  runtimeError ["invalid coords: " ++ String.join ", " args] model

{-|
take the first two items from Arguments as i and j
if they fail to parse to ints or not enough, then return width + 1,
causing Matrix.get to return Nothing and allowing errors down the chain
-}
getCoords : Argument -> Model -> (Int, Int)
getCoords args model = 
  let
    (width, height) = model.patches.size
    i = 
      case List.head args of 
        Just x -> 
          case String.toInt x of
            Ok v -> v
            Err _ -> width + 1
        Nothing -> width + 1
    j = 
      case List.head (List.drop 1 args) of 
        Just x -> 
          case String.toInt x of
            Ok v -> v
            Err _ -> height + 1
        Nothing -> height + 1
  in 
    (i, j)