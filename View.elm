module View exposing (..)

import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEv
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Model exposing (Model, Piece, gameOver, getKing)
import Msg exposing (..)

view : Model -> Html Msg
view model =
  let
   viewBoxSize = "0 0 " ++ toString(boardWidth model) ++ " " ++ toString(boardHeight model + 20)
  in
    Html.div [HtmlAttr.style [("padding", "20px")]] [
      Html.div [] [
        svg [version "1.1", width "75vmin", height "75vmin", x "0", y "0", viewBox viewBoxSize]
          ([(renderBoard model), (renderExit model)] ++ (renderPieces model))
      ],
      Html.div [] [
          (if (gameOver model) then gameOverMessage else (instructions model)), resetLink
      ]
    ]

textStyle = HtmlAttr.style [("font-family", "Verdana, Sans")]

instructions: Model -> Html Msg
instructions model =
    Html.p [textStyle]
        [text ("Playing " ++ model.name ++ ". Get the red block to the exit at the bottom." ++
        " Choose block by letter or with mouse, move with arrows keys.")]

gameOverMessage: Html Msg
gameOverMessage =
    Html.h1 [HtmlAttr.style [("color", "red")]] [text "You completed it!"]

resetLink: Html Msg
resetLink =
    Html.p [textStyle]
        [
        Html.a [HtmlAttr.href "#", HtmlEv.onClick (Reset "classic")][text "Start again (Classic)"],
        text " | ",
        Html.a [HtmlAttr.href "#", HtmlEv.onClick (Reset "easy")][text "Start again (Easy)"]
        ]

renderBoard: Model -> Svg Msg
renderBoard model =
  let
    w = toString (boardWidth model)
    h = toString (boardHeight model)
  in
    rect [width w, height h, fill "black"][]

renderExit: Model -> Svg Msg
renderExit model =
  let
    xOffset = (model.winningPos.c) * 100 - 10
    h = boardHeight model + 20
    maybeW = Maybe.map (\p -> (p.shape.width * 100) + 20) (getKing model)
    w = Maybe.withDefault 20 maybeW
  in
    rect [x (toString xOffset), width (toString w), height (toString h), fill "black"][]

boardWidth: Model -> Int
boardWidth model = model.numCols * 100

boardHeight: Model -> Int
boardHeight model = model.numRows * 100

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
