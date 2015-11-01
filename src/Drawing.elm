module Drawing where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (red, black, toRgb, rgb, Color)

import Array exposing (Array)
import Matrix
import Model exposing (..)


drawRect : Model -> Shape -> (Float, Float) -> Float -> Float -> Color -> Form
drawRect model myRect (originX, originY) x y color =
    myRect
      |> filled color
      |> move (originX + (model.patchSize * x), originY - (model.patchSize * y))

originCors : Model -> (Float, Float)
originCors model =
  let
    originX = (model.patchSize / 2) - (toFloat model.width / 2)
    originY = (toFloat model.height / 2) - model.patchSize / 2
  in
    (originX, originY)

drawArray : Model -> List Form
drawArray model =
  let
    myRect = rect model.patchSize model.patchSize
    cors = originCors model
    drawRect' x y p = drawRect model myRect cors (toFloat x) (toFloat y) p
  in
    List.map (\((i, j), p) -> drawRect' i j p.pcolor)
      <| Array.toList
      <| Matrix.toIndexedArray model.patches

drawTurtles : Model -> List Form
drawTurtles model =
  let
    myRect = rect model.patchSize model.patchSize
    cors = originCors model
    drawRect' x y p = drawRect model myRect cors (toFloat x) (toFloat y) p
  in
    List.map (\p -> drawRect' p.xcor p.ycor p.color)
      <| model.turtles

background width height =
  rect (toFloat width) (toFloat height)
    |> filled black


draw program =
  collage program.model.width program.model.height
    <| (background program.model.width program.model.height) :: (drawArray program.model ++ drawTurtles program.model)
