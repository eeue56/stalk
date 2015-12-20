module Parser.Runner where

import Model exposing (..)

import Language.Patches exposing (..)
import Language.Patches.Getters exposing (..)
import Language.Patches.Setters exposing (..)

import Language.Turtles.Setters exposing (..)
import Language.Turtles.Getters exposing (..)


import Language.Stack as Stack
import Language.Stack exposing (..)
import Language.Strings as Strings
import Language.Maths exposing (..)
import Language.Cmp exposing (..)
import Language.Flow exposing (..)

import Language.Functional exposing (..)

import String
import Debug exposing (log)
import Utils

import Parser exposing (..)
import Parser.Errors exposing (compileError, runtimeError)
import Parser.Symbols exposing (funcSplitter)
import Parser.Models exposing (Runner)


runCommand : Runner
runCommand lineNumber (command, stackUses) tempModel =
  let
    model = { tempModel | stack = List.drop stackUses tempModel.stack }

  in
      case command of
        Eval args -> runCommand lineNumber (Parser.parse (String.join "," args) model) model
        ApplyRight args -> applyRight (runCommand) args model
        ApplyLeft args -> applyLeft (runCommand) args model
        Filter args -> filter (runCommand) args model
        ReduceRight args -> reduceRight (runCommand) args model
        ReduceLeft args -> reduceLeft (runCommand) args model
        TakeWhile args -> takeWhile (runCommand) args model
        DropWhile args -> dropWhile (runCommand) args model

        SetPcolor color -> setPcolor color model
        SetPcolorOf args -> setPcolorOf args model

        PatchAt args -> patchAt args model
        NeighboursOf args -> neighboursOf args model
        PcolorOf args -> pcolorOf args model
        PxcorOf args -> pxcorOf args model
        PycorOf args -> pycorOf args model
        PxycorOf args -> pxycorOf args model

        LogPatch coors -> logPatch coors model

        Spawn args -> spawn args model
        TurtleOn args -> turtleOn args model

        CreateStacks args -> Stack.createStacks args model
        UseStack args -> Stack.use args model
        DropStack args -> Stack.drop args model
        PushToShelfStack args -> Stack.pushToShelfStack args model
        StoreStack -> Stack.storeCurrentStack model
        EmptyStack -> emptyStack model
        DropShelf -> Stack.dropAllStacks model

        PushToStack args -> pushToStack args model
        ReversePushToStack args -> pushReverseToStack args model
        PopOffStack args -> popOffStack args model
        RepeatTopOfStack args -> repeatTopOfStack args model
        SwapTopOfStack -> Stack.swap model
        BringToTopOfStack args -> bringToTopOfStack args model
        ReverseStack -> emptyStack model |> pushReverseToStack model.stack
        SizeStack -> stackSize model
        ReplaceStackItem args -> replaceStackItem args model

        StringToStack args -> Strings.toStack args model

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
        BoolToInt args -> boolToInt args model

        Label args -> addLabel args lineNumber model
        Jump args -> jumpTo args model

        CompileError messages ->
          compileError (["Error on line: " ++ toString lineNumber] ++ messages) model
        Clear ->
          clearPatches model
            |> emptyStack
            |> dropAllStacks
        Still -> model
        Failed -> runtimeError ["Failed."] model
        _ -> compileError ["Command type not found"] model

programRunner : String -> Model -> Model
programRunner enteredText model =
  let
    model' = { model | errorMessage = "" }
    commands = List.indexedMap (,) <| List.filter (not << String.isEmpty) <| String.lines enteredText
  in
    List.foldl (\(lineNumber, command) model' -> runCommand lineNumber (parse command model') model') model' commands

