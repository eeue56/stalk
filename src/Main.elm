module Main where

import Dict
import String
import Debug exposing (log)

import Model exposing (..)

import Patches exposing (..)
import Patches.Getters exposing (..)
import Patches.Setters exposing (..)

import Stack exposing (..)
import Maths exposing (..)
import Cmp exposing (..)

import Views exposing (..)

import Parser exposing (..)
import Parser.Errors exposing (compileError, runtimeError)


commands : CommandLibrary
commands = Dict.fromList
  [("clear", always Clear),
   ("still", always Still),
   ("failed", always Failed),
   ("error", CompileError),
   ("eval", Eval),
   ("apply", Apply),

   -- stack operations
   ("empty-stack", always EmptyStack),
   ("push", PushToStack),
   ("pop", PopOffStack),
   ("repeat", RepeatTopOfStack),
   ("swap", always SwapTopOfStack),
   ("top", BringToTopOfStack),

   -- setters to do with patches
   ("set-pcolor", SetPcolor),
   ("set-pcolor-of", SetPcolorOf),

   -- patch getters
   ("patch-at", PatchAt),
   ("neighbours-of", NeighboursOf),
   ("pcolor-of", PcolorOf),
   ("pxcor-of", PxcorOf),
   ("pycor-of", PycorOf),
   ("pxycor-of", PxycorOf),

   -- patch loggers
   ("log-patch", LogPatch),

   -- math operations
   ("add", Add),
   ("+", Add),
   ("subtract", Subtract),
   ("-", Subtract),
   ("multiply", Multiply),
   ("*", Multiply),
   ("divide", Divide),
   ("/", Divide),
   ("++", Increment),
   ("--", Decrement),

   -- comparison operations
   ("eq", Equals),
   ("==", Equals),
   ("not-eq", NotEquals),
   ("/=", NotEquals),

   ("lt", LessThan),
   ("<", LessThan),
   ("lt-or-eq", LessThanOrEquals),
   ("<=", LessThanOrEquals),

   ("mt", MoreThan),
   (">", MoreThan),
   ("mt-or-eq", MoreThanOrEquals),
   (">=", MoreThanOrEquals),

   ("true", TrueTest),
   ("false", FalseTest)

  ]

apply : Argument -> Model -> Model
apply args model = 
  let 
    command extraArgs = Parser.parse (String.join " , " <| args ++ [extraArgs]) model
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

update : Action -> Model -> Model
update action model = 
  case action of 
    UpdateText x -> { model | enteredText <- x } 
    Enter -> 
      let
        model' = { model | errorMessage <- "" }
        commands = List.indexedMap (,) <| List.filter (not << String.isEmpty) <| String.lines model'.enteredText 
      in
        List.foldl (\(lineNumber, command) model'' -> runCommand lineNumber (parse command model'') model'') model' commands
    Reset -> runCommand 0 (Clear, 0) model
    Noop -> model

model' : Model
model' = {
  enteredText = "",
  errorMessage = "",
  patches = defaultPatches 25 25,
  commands = commands,
  width = 750,
  height = 750,
  stack = [],
  patchSize =  750 / 25 }

enteredCommands : Signal.Mailbox Action
enteredCommands = Signal.mailbox Noop

model = Signal.foldp
  update
  model'
  enteredCommands.signal

main = Signal.map (view enteredCommands.address) model