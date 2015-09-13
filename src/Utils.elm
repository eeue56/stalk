module Utils where

import String
import Debug exposing (log)
import Color exposing (Color, black, red, green, blue, rgb)
import Matrix
import Array

alwaysOkInt : String -> Int
alwaysOkInt v = 
  case String.toInt v of 
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



levenshtein : String -> String -> Int
levenshtein s1' s2' =
  let
    unsafeGet i j m = case Matrix.get i j m of Just v -> v
    unsafeConcatV r m = case Matrix.concatVertical r m of Just v -> v
    unsafeConcatH c m = case Matrix.concatHorizontal c m of Just v -> v
    unsafeFromList xs = case Matrix.fromList xs of Just v -> v
    s1 = Array.fromList <| String.toList s1'
    s2 = Array.fromList <| String.toList s2'
    l1 = Array.length s1
    l2 = Array.length s2
    cost i j = if Array.get (i-1) s1 == Array.get (j-1) s2 then 0 else 1
    levInversion i j m = if i > 1 && 
                       j > 1 && 
                       Array.get (i-1) s1 == Array.get (j-2) s2 &&
                       Array.get (j-2) s1 == Array.get (j-1) s2
                    then min (levStep i j m) (unsafeGet (i-2) (j-2) m + 1)
                    else levStep i j m

    levStep : Int -> Int -> Matrix.Matrix Int -> Int
    levStep i j m = case List.minimum [ unsafeGet (i-1) j m + 1
                                      , unsafeGet i (j-1) m + 1
                                      , unsafeGet (i-1) (j-1) m + (cost i j) ]
                    of Just v -> v

    init : Matrix.Matrix Int
    init = unsafeConcatH
            (unsafeFromList <| List.map (\x->[x]) [0..l2]) <|
            unsafeConcatV
                (unsafeFromList [[1..l1]])
                (Matrix.repeat l1 l2 0)

    step : Int -> Matrix.Matrix Int -> Matrix.Matrix Int
    step i acc = List.foldl (\j m -> Matrix.set i j (levInversion i j m) m) acc [1..l2]
  in
    unsafeGet l1 l2 (List.foldl step init [1..l1])