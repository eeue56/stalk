module Parser.Runner where

import Model exposing (..)

import Language.Patches exposing (..)
import Language.Patches.Getters exposing (..)
import Language.Patches.Setters exposing (..)

import Language.Stack as Stack
import Language.Stack exposing (..)
import Language.Maths exposing (..)
import Language.Cmp exposing (..)

import Views exposing (..)
import String

import Parser exposing (..)
import Parser.Errors exposing (compileError, runtimeError)


{- 
  When no args, there should be a dollar, empty list
  When the args is just the function name (len args == 1) then it should the func name + dollar, empty list
  Otherwise, there should be an empty string, the args
  
  This assumes that more than one args means that a paritial function has been provided,
  meaning that there's no need for a dollar
  -}
functionPartition : Argument -> (String, Argument)
functionPartition args = 
  case args of 
    [] -> ("$", [])
    x::[] -> (x ++ " $ ", [])
    x::xs -> ("", args)


apply : Argument -> Model -> Model
apply args model = 
  let 
    command extraArgs = 
      let
        (func, args') = functionPartition args
      in
        Parser.parse (func ++ (String.join " , " <| args' ++ [extraArgs])) model
  in
  List.foldl (\extra model -> runCommand 0 (command extra) model) (Stack.emptyStack model) model.stack

runCommand : Int -> (Command, Int) -> Model -> Model
runCommand lineNumber (command, stackUses) model' =
  let
    model = { model' | stack <- List.drop stackUses model'.stack }
  in
    case command of
      Eval args -> runCommand lineNumber (Parser.parse (String.join "," args) model) model
      Apply args -> apply args model 

      SetPcolor color -> setPcolor color model
      SetPcolorOf args -> setPcolorOf args model

      PatchAt args -> patchAt args model
      NeighboursOf args -> neighboursOf args model
      PcolorOf args -> pcolorOf args model
      PxcorOf args -> pxcorOf args model
      PycorOf args -> pycorOf args model
      PxycorOf args -> pxycorOf args model

      LogPatch coors -> logPatch coors model

      EmptyStack -> emptyStack model
      PushToStack args -> pushToStack args model
      PopOffStack args -> popOffStack args model
      RepeatTopOfStack args -> repeatTopOfStack args model
      SwapTopOfStack -> Stack.swap model
      BringToTopOfStack args -> bringToTopOfStack args model

      Add args -> add args model
      Subtract args -> subtract args model
      Multiply args -> multiply args model
      Divide args -> divide args model
      Increment args -> increment args model
      Decrement args -> decrement args model

      Equals args -> eq args model
      NotEquals args -> notEq args model
      MoreThan args -> moreThan args model
      MoreThanOrEquals args -> moreThanOrEquals args model
      LessThan args -> lessThan args model
      LessThanOrEquals args -> lessThanOrEquals args model
      TrueTest args -> true args model
      FalseTest args -> false args model

      CompileError messages -> compileError (["Error on line: " ++ toString lineNumber] ++ messages) model
      Clear -> clearPatches model |> emptyStack
      Still -> model
      Failed -> runtimeError ["Failed."] model
      _ -> compileError ["Command type not found"] model