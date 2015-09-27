module Views where

import Html exposing (Html, Attribute, div, textarea, button, fromElement, text)
import Html.Attributes exposing (placeholder, value, hidden, style)

import Html.Events exposing (keyCode, on, targetValue, onClick)

import Json.Decode as Json
import String
import Model exposing (..)
import Drawing exposing (..)

drawWorld program =
  draw program

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err ""

onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)

errorArea : Program -> Html
errorArea program =
  let 
    stripedMessage = String.trim program.model.errorMessage
  in
    textarea [ 
      value <| stripedMessage, 
      hidden <| stripedMessage == "",
      style 
        [ ("height", "250px"), 
          ("width", "500px")
        ]
      ] []

commandsTextarea : Signal.Address Action -> Program -> Html
commandsTextarea address program = 
  div [ style [ ("width", "100%")]] 
    [
      textarea 
        [ 
          placeholder "Enter a command",
          on "input" targetValue (Signal.message address << UpdateText),
          style 
            [ ("height", "250px"), 
              ("width", (toString program.model.width) ++ "px")
            ]
        ] [],
      button [ onClick address Enter ] [ text "Run program" ],
      button [ onClick address Reset ] [ text "Reset" ],
      button [ onClick address Step ] [ text <| "Step through: " ++ (toString program.steps) ],
      errorArea program
    ]    

view address program =
  div [] [
    div [ ] 
      [
        commandsTextarea address program
      ],
    fromElement <| drawWorld program,
    text <| toString program.model.stack
  ]