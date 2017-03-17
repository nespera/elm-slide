import Html exposing (div, Html)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Dict exposing (..)
import Keyboard exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Piece = { name: String, shape: Shape, position: Position}
type alias Shape = {width : Int, height : Int, color: String}
type alias Position = { r: Int, c: Int }
type alias Model = {pieces: Dict String Piece, active: String}

big: Shape
big = {width = 2, height = 2, color = "crimson"}

small: Shape
small = {width = 1, height = 1, color = "green"}

tall: Shape
tall = {width = 1, height = 2, color = "teal"}

wide: Shape
wide = {width = 2, height = 1, color = "orange"}


init : (Model, Cmd Msg)
init =
  ({pieces = Dict.fromList [
    ("king", {name = "king", shape = big, position = {r = 0, c = 1}}),
    ("tall1", {name = "tall1", shape = tall, position = {r = 0, c = 0}}),
    ("tall2", {name = "tall2", shape = tall, position = {r = 0, c = 3}}),
    ("tall3", {name = "tall3", shape = tall, position = {r = 2, c = 0}}),
    ("tall4", {name = "tall4", shape = tall, position = {r = 2, c = 3}}),
    ("wide", {name = "wide", shape = wide, position = {r = 2, c = 1}}),
    ("pawn1", {name = "pawn1", shape = small, position = {r = 4, c = 0}}),
    ("pawn2", {name = "pawn2", shape = small, position = {r = 3, c = 1}}),
    ("pawn3", {name = "pawn3", shape = small, position = {r = 3, c = 2}}),
    ("pawn4", {name = "pawn4", shape = small, position = {r = 4, c = 3}})
  ], active = "king"}, Cmd.none)



-- UPDATE


type Msg
  = Choose String | Pressed KeyCode

type Direction
  = Up | Down | Right | Left

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Choose name ->
      ({model | active = name}, Cmd.none)
    Pressed keyCode ->
      (handleKeyPress model keyCode, Cmd.none)

handleKeyPress: Model -> KeyCode -> Model
handleKeyPress model keyCode =
  case keyCode of
    110 ->  nextPiece model
    37 -> makeMove model Left
    38 -> makeMove model Up
    39 -> makeMove model Right
    40 -> makeMove model Down
    _ -> model

makeMove: Model -> Direction -> Model
makeMove model direction =
  let
    activePiece = Dict.get model.active model.pieces
  in
    case activePiece of
      Just piece ->
        let
          newPosition = updatePosition piece.position direction
        in
         updatePiece model model.active {piece | position = newPosition}
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

nextPiece: Model -> Model
nextPiece model =
  let
    names = List.sort (Dict.keys model.pieces)
    nextOne = List.head (dropUntil (names ++ names) model.active)
  in
    case nextOne of
      Just piece ->
         {model | active = piece}
      _ -> model


dropUntil: List String -> String -> List String
dropUntil list item =
  case list of
    other :: tail ->
      if (other == item) then tail else (dropUntil tail item)
    [] -> []

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
      downs (\code -> Pressed code),
      presses (\code -> Pressed code)
    ]



-- VIEW


view : Model -> Html Msg
view model =
  Html.div [Html.Attributes.style [("margin", "20px")]] [
    svg [version "1.1", width "400", height "500", x "0", y "0", viewBox "0 0 400 500"] (
      (rect [width "100%", height "100%", fill "black"][])
      :: renderPieces model
      )
      --,circle [cx "150", cy "100", r "80", fill "green"][]
      --,text_ [x "150", y "125", fontSize "60", textAnchor "middle", fill "white"][text "SVG"]
  ]

renderPieces: Model -> List (Svg Msg)
renderPieces model =
  let
    pieces = (Dict.values model.pieces)
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
  in
    rect [
      x (toString(100 * piece.position.c)),
      y (toString(100 * piece.position.r)),
      width (toString (100 * piece.shape.width)),
      height (toString (100 * piece.shape.height)),
      stroke "black",
      strokeWidth "5",
      fill piece.shape.color,
      onClick (Choose piece.name)
  ] animate
