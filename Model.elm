module Model exposing (Model, Position, Piece, valid, initial, allPieces, gameOver)

import Dict exposing (Dict)
import Set

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


initial : Model
initial =
  {pieces = Dict.fromList [
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
  ], active = "a"}

allPieces: Model -> List Piece
allPieces model =
  Dict.values model.pieces

valid: Model -> Bool
valid model =
  inbounds model && noOverlaps model

inbounds: Model -> Bool
inbounds model =
  let
    positions = List.concatMap coverage (allPieces model)
    rows = List.map (\position -> position.r) positions
    maxRow = Maybe.withDefault 0 (List.maximum rows)
    minRow = Maybe.withDefault 0 (List.minimum rows)
    columns = List.map (\position -> position.c ) positions
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

gameOver: Model -> Bool
gameOver model =
  let
    king = Dict.get "a" model.pieces
   in
     case king of
       Just piece -> piece.position.c == 1 && piece.position.r == 3
       _ -> False