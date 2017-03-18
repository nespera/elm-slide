import Html exposing (program)
import Dict exposing (Dict)
import Keyboard exposing (KeyCode, downs, presses)
import Char
import Model exposing (Model, Position, Piece)
import Msg exposing (..)
import View exposing (view)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init = (Model.initial, Cmd.none)

-- UPDATE

type Direction
  = Up | Down | Right | Left

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  if (Model.gameOver model)
    then (model, Cmd.none)
  else
    case msg of
      Choose name ->
        ({model | active = name}, Cmd.none)
      Pressed keyCode ->
        (handleKeyPress model keyCode, Cmd.none)

handleKeyPress: Model -> KeyCode -> Model
handleKeyPress model keyCode =
  let
    char = keyCode |> Char.fromCode |> String.fromChar |> String.toLower
    names = Dict.keys model.pieces
  in
  case keyCode of
    37 -> makeMove model Left
    38 -> makeMove model Up
    39 -> makeMove model Right
    40 -> makeMove model Down
    _ ->  if (List.member char names) then {model | active = char} else model

makeMove: Model -> Direction -> Model
makeMove model direction =
  let
    activePiece = Dict.get model.active model.pieces
  in
    case activePiece of
      Just piece ->
        let
          newPosition = updatePosition piece.position direction
          movedPiece = {piece | position = newPosition}
          newModel = updatePiece model model.active movedPiece
        in
          if Model.valid newModel then newModel else model
      _  -> model

updatePosition: Position -> Direction -> Position
updatePosition position direction =
  case direction of
    Up -> {position | r = position.r - 1}
    Down -> {position | r = position.r + 1}
    Left -> {position | c = position.c - 1}
    Right -> {position | c = position.c + 1}

updatePiece: Model -> String -> Piece -> Model
updatePiece model name piece =
    {model | pieces = (Dict.insert name piece model.pieces)}


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
      downs (\code -> Pressed code),
      presses (\code -> Pressed code)
    ]


