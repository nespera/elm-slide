import Html exposing (div, Html)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Dict exposing (..)
import Color exposing (..)
import Keyboard exposing (..)
import Char exposing (fromCode)

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
wide = {width = 2, height = 1, color = "purple"}


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
  = Choose String | Pressed Char | NextPiece


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Choose name ->
      ({model | active = name}, Cmd.none)
    Pressed char ->
      (if (char == 'n') then next(model) else model, Cmd.none)
    NextPiece ->
      (model, Cmd.none)

next: Model -> Model
next model =
  let
    names = List.sort (Dict.keys model.pieces)
    nextOne = List.head (dropUntil (names ++ names) model.active)
  in
    case nextOne of
      Just piece ->
         {model | active = piece}
      Nothing -> model


dropUntil: List String -> String -> List String
dropUntil list item =
  case list of
    other :: tail ->
      if (other == item) then tail else (dropUntil tail item)
    [] -> []

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    presses (\code -> Pressed (Char.fromCode code))



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
