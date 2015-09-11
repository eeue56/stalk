module Drawing where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (red, black, toRgb, rgb)

import Array exposing (Array)
import Matrix
import Model exposing (..)


drawRect : Model -> Shape -> (Float, Float) -> Float -> Float -> Patch -> Form
drawRect model myRect (originX, originY) x y p = 
    myRect
      |> filled p.pcolor
      |> move (originX + (model.patchSize * x), originY - (model.patchSize * y))

drawArray : Model -> List Form
drawArray model =
  let
    myRect = rect model.patchSize model.patchSize 
    originX = (model.patchSize / 2) - (toFloat model.width / 2) 
    originY = (toFloat model.height / 2) - model.patchSize / 2
    drawRect' x y p = drawRect model myRect (originX, originY) (toFloat x) (toFloat y) p
  in
    List.map (\((i, j), p) -> drawRect' i j p)
      <| Array.toList
      <| Matrix.toIndexedArray model.patches



background width height =
  rect (toFloat width) (toFloat height)
    |> filled black

--draw : Model -> Element
draw model = 
  collage model.width model.height
    <| (background model.width model.height) :: (drawArray model)