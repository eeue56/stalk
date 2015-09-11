module Model where

import Matrix exposing (Matrix)
import Color exposing (Color)


type Command = Clear 
  | Enter
  | UpdateText String
  | Still

type alias Patch = {
  pcolor: Color
}

type alias CommandPanel = (String, Command)

type alias Model = {
  enteredText : String,
  
  patches : Matrix Patch,
  commands : List CommandPanel,
  
  width : Int,
  height : Int,

  patchSize : Float }
