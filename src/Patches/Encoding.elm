module Patches.Encoding where

import Dict
import Color exposing (Color, black, red, green, blue, rgb)
import String

import Utils exposing (..)
import Model exposing (..)
import Parser exposing (runtimeError)


patchColorAsString : Patch -> List String
patchColorAsString p =
  let
    color = Color.toRgb p.pcolor
  in
    List.map toString <| List.reverse [color.red, color.green, color.blue]

patchAsString : Patch -> String
patchAsString patch =
  String.join "~" [
    "pcolor = " ++ (String.join " " <| patchColorAsString patch),
    "pxcor = " ++ (toString patch.pxcor),
    "pycor = " ++ (toString patch.pycor)
  ]

patchFromString : String -> Maybe Patch
patchFromString patchString = 
  let
    bits = String.split "~" patchString
    asDict = 
      Dict.fromList
      <| List.map (\xs -> 
        case xs of
          x::y::[] -> (x, y)
          _ -> ("", ""))
      <| List.map (String.split " = ") bits
  in
    case Dict.get "pxcor" asDict of 
      Just pxcor ->
        case Dict.get "pxcor" asDict of
          Just pycor -> 
            case Dict.get "pcolor" asDict of
              Just pcolor -> 
                Just { pxcor = alwaysOkInt pxcor, 
                       pycor = alwaysOkInt pycor,
                       pcolor = rgbFromList <| String.split " " pcolor }
              Nothing -> Nothing
          Nothing -> Nothing
      Nothing -> Nothing