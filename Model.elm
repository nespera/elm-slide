module Model exposing (Model, Position, Piece, valid, initial, gameOver, getPiece, updatePiece)

import Set

type alias Piece = { name: String, shape: Shape, position: Position}
type alias Shape = {width : Int, height : Int, color: String}
type alias Position = { r: Int, c: Int }
type alias Model = {name:String, numRows: Int, numCols: Int, pieces: List Piece, active: String,
  king: String, winningPos: Position}

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
  {
    name = "Classic Klotski",
    numRows = 5,
    numCols = 4,
    active = "a",
    king = "a",
    winningPos = {r = 3, c = 1},
    pieces = [
        {name = "a", shape = big, position = {r = 0, c = 1}},
        {name = "b", shape = tall, position = {r = 0, c = 0}},
        {name = "c", shape = tall, position = {r = 0, c = 3}},
        {name = "d", shape = tall, position = {r = 2, c = 0}},
        {name = "e", shape = tall, position = {r = 2, c = 3}},
        {name = "f", shape = wide, position = {r = 2, c = 1}},
        {name = "g", shape = small, position = {r = 4, c = 0}},
        {name = "h", shape = small, position = {r = 3, c = 1}},
        {name = "i", shape = small, position = {r = 3, c = 2}},
        {name = "j", shape = small, position = {r = 4, c = 3}}
        ]
  }

getPiece: Model -> String -> Maybe Piece
getPiece model name =
  List.head (List.filter (isCalled name) model.pieces)

getKing: Model -> Maybe Piece
getKing model = getPiece model model.king

updatePiece: Model -> Piece -> Model
updatePiece model movedPiece =
    let
      otherPieces = List.filter (\p -> p.name /= movedPiece.name) model.pieces
    in
      {model | pieces = movedPiece :: otherPieces}

valid: Model -> Bool
valid model =
  noneOutsideBoard model && noOverlaps model

noneOutsideBoard: Model -> Bool
noneOutsideBoard model =
  let
    positions = List.concatMap coverage (model.pieces)
  in
    List.all (onTheBoard model) positions

onTheBoard: Model -> Position -> Bool
onTheBoard model pos =
  pos.r >= 0 && pos.r < model.numRows && pos.c >= 0 && pos.c < model.numCols

noOverlaps: Model -> Bool
noOverlaps model =
  let
    pieces = model.pieces
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
    king = getKing model
   in
     case king of
       Just piece -> piece.position == model.winningPos
       _ -> False

isCalled: String -> Piece -> Bool
isCalled name piece =
    piece.name == name