module Model where

import Matrix exposing (Matrix)
import Color exposing (Color)
import Dict

type alias Argument = List String

type Action = Reset
  | Enter
  | UpdateText String
  | Noop

type Command = Clear 
  | SetPcolor Argument
  | SetPcolorOf Argument 
  | PcolorOf Argument
  | PxcorOf Argument
  | PycorOf Argument
  | PxycorOf Argument
  | LogPatch Argument

  | EmptyStack Argument
  | PushToStack Argument
  | Still
  | Failed

type alias CommandLibrary = Dict.Dict String (Argument -> Command)

type alias Patch = {
  pcolor: Color,
  pxcor : Int,
  pycor : Int
}

type alias CommandPanel = (String, Command)

type alias Model = {
  enteredText : String,

  patches : Matrix Patch,
  commands : CommandLibrary,
  
  width : Int,
  height : Int,

  stack : List String,

  patchSize : Float }
