module Model where

import Matrix exposing (Matrix)
import Color exposing (Color)
import Dict

type alias Argument = List String


type Command = Clear 
  | Enter
  | UpdateText String
  | SetPcolor Argument 
  | Still
  | Failed
  
type alias CommandLibrary = Dict.Dict String (Argument -> Command)

type alias Patch = {
  pcolor: Color
}

type alias CommandPanel = (String, Command)

type alias Model = {
  enteredText : String,

  patches : Matrix Patch,
  commands : CommandLibrary,
  
  width : Int,
  height : Int,

  patchSize : Float }
