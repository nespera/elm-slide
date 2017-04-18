module View exposing (..)

import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEv
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Model exposing (Model, Piece, gameOver)
import Msg exposing (..)

view : Model -> Html Msg
view model =
  Html.div [HtmlAttr.style [("padding", "20px")]] [
    Html.div [] [
      svg [version "1.1", width "80vmin", height "80vmin", x "0", y "0", viewBox "0 0 400 520"]
        (renderBoard ++ (renderPieces model))
    ],
    Html.div [] [
        (if (gameOver model) then gameOverMessage else instructions), resetLink
    ]
  ]

textStyle = HtmlAttr.style [("font-family", "Verdana, Sans")]

instructions: Html Msg
instructions =
    Html.p [textStyle]
        [text "Get the red block to the exit at the bottom. Choose block by letter or with mouse, move with arrows keys."]

gameOverMessage: Html Msg
gameOverMessage =
    Html.h1 [HtmlAttr.style [("color", "red")]] [text "You completed it!"]

resetLink: Html Msg
resetLink =
    Html.p [textStyle]
        [Html.a [HtmlAttr.href "#", HtmlEv.onClick Reset][text "Start again"]]

renderBoard: List (Svg Msg)
renderBoard = [
    rect [width "400", height "500", fill "black"][] ,
    rect [x "90", width "220", height "520", fill "black"][]
  ]

renderPieces: Model -> List (Svg Msg)
renderPieces model =
    List.map (renderPiece model) model.pieces

flash: Svg Msg
flash = animate [
    attributeType "XML",  attributeName "fill-opacity", from "1", to "0.5", dur "1s", repeatCount "indefinite"
  ][]

renderPiece: Model -> Piece -> Svg Msg
renderPiece model piece =
  let
    isActive = (piece.name == model.active)
    animate = if (isActive && not(gameOver model)) then [flash] else []
    xPos = 100 * piece.position.c
    yPos = 100 * piece.position.r
    w = 100 * piece.shape.width
    h = 100 * piece.shape.height
    xMid = xPos + (w//2)
    yMid = yPos + (h//2)
    opacity = if (gameOver model) then 0.5 else 1.0
  in
    g[onClick (Choose piece.name)][
      rect [
        x (toString xPos),
        y (toString yPos),
        width (toString w),
        height (toString h),
        stroke "black",
        strokeWidth "5",
        fill piece.shape.color,
        fillOpacity (toString opacity)
      ] animate,
      text_ [
        x (toString xMid),
        y (toString (yMid + 10)),
        fontSize "50",
        textAnchor "middle",
        fill "white"
      ][text piece.name]
    ]
