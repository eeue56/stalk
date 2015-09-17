module Utils where

import String
import Debug exposing (log)
import Color exposing (Color, black, red, green, blue, rgb)
import Matrix
import Array
import Convert

alwaysOkInt : String -> Int
alwaysOkInt v = 
  case String.toInt v of 
    Ok x -> x
    Err _ -> log "Incorrect convert" 0

-- TODO: add to elm-simple-data
rgbFromList vals = 
  case vals of 
    r::g::b::[] -> rgb (alwaysOkInt r) (alwaysOkInt g) (alwaysOkInt b)
    _ -> rgb -1 -1 -1

splitFirst : String -> String -> (String, String)
splitFirst spliter string =
  let 
    locs = String.indexes spliter string
  in
    case locs of 
      [] -> (string, "")
      x::xs -> (String.left x string, String.dropLeft (x + 1) string)
      _ -> (string, "")

serializeRecord : String -> String
serializeRecord record =
  let
    -- remove braces
    record' = 
      String.join "~"
      <| String.split ", "
      <| String.trim 
      <| String.dropLeft 1 
      <| String.dropRight 1 record
  in
    record'


levenshtein : String -> String -> Int
levenshtein s1' s2' =
  let
    unsafeGet i j m = Convert.defaultMaybe (Matrix.get i j) 999 m
    unsafeConcatV r m = Convert.defaultMaybe (Matrix.concatVertical r) m m 
    unsafeConcatH c m = Convert.defaultMaybe (Matrix.concatHorizontal c) m m
    unsafeFromList xs = Convert.defaultMaybe (Matrix.fromList) (Matrix.empty) xs 
    s1 = Array.fromList <| String.toList s1'
    s2 = Array.fromList <| String.toList s2'
    l1 = Array.length s1
    l2 = Array.length s2
    cost i j = if Array.get (i-1) s1 == Array.get (j-1) s2 then 0 else 1
    levInversion i j m = if i > 1 && 
                       j > 1 && 
                       Array.get (i-1) s1 == Array.get (j-2) s2 &&
                       Array.get (j-2) s1 == Array.get (j-1) s2
                    then min (levStep i j m) ((unsafeGet (i-2) (j-2) m) + 1) 
                    else levStep i j m

    levStep : Int -> Int -> Matrix.Matrix Int -> Int
    levStep i j m = 
      case 
        List.minimum  
          [ unsafeGet (i-1) j m + 1
          , unsafeGet i (j-1) m + 1
          , unsafeGet (i-1) (j-1) m + (cost i j) ] 
      of
       Just v -> v
       Nothing -> 0

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