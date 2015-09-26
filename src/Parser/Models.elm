module Parser.Models where

import Model exposing (..)

type alias Runner = (Int -> (Command, Int) -> Model -> Model)