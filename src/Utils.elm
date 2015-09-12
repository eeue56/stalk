module Utils where

import String
import Debug exposing (log)
import Color exposing (Color, black, red, green, blue, rgb)

alwaysOkInt v = case String.toInt v of 
  Ok x -> x
  Err _ -> log "Incorrect convert" 0

-- TODO: add to elm-simple-data
rgbFromList vals = 
  let
    r = alwaysOkInt <| case List.head vals of Just v -> v
    g = alwaysOkInt <| (\x -> case List.head x of Just v -> v) 
      <| (case List.tail vals of Just v -> v)
    b = alwaysOkInt <| case List.head <| List.reverse vals of Just v -> v
    
  in
    rgb r g b