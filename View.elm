module View exposing (..)

import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Model exposing (Model, Piece)
import Msg exposing (..)

view : Model -> Html Msg
view model =
  Html.div [HtmlAttr.style [("padding", "20px")]] [
    Html.div [] [
      svg [version "1.1", width "400", height "520", x "0", y "0", viewBox "0 0 400 520"]
        (renderBoard ++ (renderPieces model))
    ],
    Html.div [][
      Html.p [] [text "Get the red block to the exit at the bottom."],
      Html.p [] [text "Choose block by letter or with mouse, move with arrows keys."]
    ]
  ]

renderBoard: List (Svg Msg)
renderBoard = [
    rect [width "400", height "500", fill "black"][] ,
    rect [x "90", width "220", height "520", fill "black"][]
  ]

renderPieces: Model -> List (Svg Msg)
renderPieces model =
  let
    pieces = (Model.allPieces model)
  in
    List.map (renderPiece model.active) pieces

flash: Svg Msg
flash = animate [
    attributeType "XML",  attributeName "fill-opacity", from "1", to "0.5", dur "1s", repeatCount "indefinite"
  ][]

renderPiece: String -> Piece -> Svg Msg
renderPiece active piece =
  let
    isActive = (piece.name == active)
    animate = if (isActive) then [flash] else []
    xPos = 100 * piece.position.c
    yPos = 100 * piece.position.r
    w = 100 * piece.shape.width
    h = 100 * piece.shape.height
    xMid = xPos + (w//2)
    yMid = yPos + (h//2)
  in
    g[onClick (Choose piece.name)][
      rect [
        x (toString xPos),
        y (toString yPos),
        width (toString w),
        height (toString h),
        stroke "black",
        strokeWidth "5",
        fill piece.shape.color
      ] animate,
      text_ [x (toString xMid), y (toString (yMid + 10)), fontSize "50", textAnchor "middle", fill "white"][text piece.name]
    ]
