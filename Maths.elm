module Maths where

import Utils exposing (alwaysOkInt)
import Stack exposing (pushToStack)
import Model exposing (..)


add : Argument -> Model -> Model
add numbers model =
  let 
    sum = List.sum <| List.map (alwaysOkInt) numbers
  in 
    pushToStack [toString sum] model

subtract : Argument -> Model -> Model
subtract numbers model =
  let 
    numbers' = List.map (alwaysOkInt) numbers
    first = List.take 1 numbers'
    others = List.map (\x -> -x) <| List.drop 1 numbers'
    sum = 
        List.sum <| first ++ others 
  in 
    pushToStack [toString sum] model