module Stack where

import Model exposing (..)
import Utils exposing (..)


emptyStack : Argument -> Model -> Model
emptyStack _ model = 
    { model | stack <- [] }

popOffStack : Argument -> Model -> Model
popOffStack _ model =
    model

pushToStack : Argument -> Model -> Model
pushToStack args model =
    { model | stack <- args ++ model.stack}