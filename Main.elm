import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Dict exposing (Dict)
import Keyboard exposing (KeyCode, downs, presses)
import Set
import Char

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

numRows = 5
numCols = 4

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
    ("a", {name = "a", shape = big, position = {r = 0, c = 1}}),
    ("b", {name = "b", shape = tall, position = {r = 0, c = 0}}),
    ("c", {name = "c", shape = tall, position = {r = 0, c = 3}}),
    ("d", {name = "d", shape = tall, position = {r = 2, c = 0}}),
    ("e", {name = "e", shape = tall, position = {r = 2, c = 3}}),
    ("f", {name = "f", shape = wide, position = {r = 2, c = 1}}),
    ("g", {name = "g", shape = small, position = {r = 4, c = 0}}),
    ("h", {name = "h", shape = small, position = {r = 3, c = 1}}),
    ("i", {name = "i", shape = small, position = {r = 3, c = 2}}),
    ("j", {name = "j", shape = small, position = {r = 4, c = 3}})
  ], active = "a"}, Cmd.none)

allPieces: Model -> List Piece
allPieces model =
  Dict.values model.pieces

valid: Model -> Bool
valid model =
  inbounds model && noOverlaps model

inbounds: Model -> Bool
inbounds model =
  let
    positions = List.map (\piece -> piece.position) (allPieces model)
    rows = List.map (\position -> position.r) positions
    maxRow = Maybe.withDefault 0 (List.maximum rows)
    minRow = Maybe.withDefault 0 (List.minimum rows)
    columns = List.map (\position -> position.c )positions
    maxCol = Maybe.withDefault 0 (List.maximum columns)
    minCol = Maybe.withDefault 0 (List.minimum columns)
  in
    minRow >= 0 && maxRow < numRows && minCol >= 0 && maxCol < numCols


noOverlaps: Model -> Bool
noOverlaps model =
  let
    pieces = (allPieces model)
    allCoverage = (List.map coverage pieces) |> List.concat
    noDupes = dropDuplicates allCoverage
  in
    List.length allCoverage == List.length noDupes

coverage: Piece -> List Position
coverage piece =
  let
    r = piece.position.r
    allRows = List.range r (piece.shape.height - 1 + r)
    c = piece.position.c
    allCols = List.range c (piece.shape.width - 1 + c)
    tuples = cartesian allRows allCols
  in
    List.map (\(r,c) -> {r = r, c = c}) tuples

cartesian : List a -> List b -> List (a,b)
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs

dropDuplicates : List Position -> List Position
dropDuplicates list =
  let
    step next (set, acc) =
      let
        hash = next.c + (next.r * 1000)
      in
        if Set.member hash set
          then (set, acc)
          else (Set.insert hash set, next::acc)
  in
    List.foldl step (Set.empty, []) list |> Tuple.second |> List.reverse

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
          if valid newModel then newModel else model
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


-- VIEW


view : Model -> Html Msg
view model =
  Html.div [Html.Attributes.style [("padding", "20px")]] [
    svg [version "1.1", width "400", height "520", x "0", y "0", viewBox "0 0 400 520"]
      (renderBoard ++ (renderPieces model))
  ]

renderBoard: List (Svg Msg)
renderBoard = [
    rect [width "400", height "500", fill "black"][] ,
    rect [x "90", width "220", height "520", fill "black"][]
  ]

renderPieces: Model -> List (Svg Msg)
renderPieces model =
  let
    pieces = (allPieces model)
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
