module Elm_2048 exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Random exposing (Generator)
import Time
import Debug
import Html.Attributes
import Array
import Json.Decode as Decode
import List.Extra

-----------------------------------------------------
-- Types and Aliases
-----------------------------------------------------

type alias Grid = List (List Num)

type alias Model =
  { board : Grid }

type Direction = Left | Right | Up | Down | Other | NewGame

type Msg = Tick | RandomPlay Play | Keystroke Direction | Gameover

type alias Play =
  {index : Index, num : Num}

-- top left = {0,0}
type alias Loc = { row:Int, col:Int }

type alias Num = Int
type alias Index = Int

type alias Flags = ()
-----------------------------------------------------
log : String -> a -> a
log s x =
  let stringX = Debug.toString x
  in case (Debug.log s stringX) of
    _ -> x


-----------------------------------------------------
-- List Helper Functions
-----------------------------------------------------

-- find elem at index i
indexList : Index -> List Num -> Num
indexList i ls =
  case (i, ls) of
    (_, [])      -> -1
    (0, l::_)    -> l
    (_, _::rest) -> indexList (i-1) rest

-----------------------------------------------------
-- MVC Functions
-----------------------------------------------------

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Flags -> (Model, Cmd Msg)
init () = (initModel, Cmd.none)

initModel : Model
initModel = {board=emptyBoard}
  -- let model = {board=emptyBoard}
  --     (onePiece, _) = update Tick model
  --     -- (twoPiece, _) = update Tick onePiece
  -- in
  --     onePiece

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Keystroke dir ->
      case (log "dir" dir) of
        Left  ->
          if canMoveLeft model.board then
            let newBoard = left model.board
            in update Tick {board = newBoard}
          else (model, Cmd.none)
        Right ->
          if canMoveRight model.board then
            let newBoard = right model.board
            in update Tick {board = newBoard}
          else (model, Cmd.none)
        Up    ->
          if canMoveUp model.board then
            let newBoard = up model.board
            in update Tick {board = newBoard}
          else (model, Cmd.none)
        Down  ->
          if canMoveDown model.board then
            let newBoard = down model.board
            in update Tick {board = newBoard}
          else (model, Cmd.none)
        NewGame ->
          let (onePiece, cmd1) =  update Tick model
              (twoPiece, cmd2) =  update Tick onePiece
          in (twoPiece, Cmd.batch [cmd1, cmd2])
        Other -> (model, Cmd.none)
    Tick ->
      let empties = findEmpties model.board
      in (model, Random.generate RandomPlay (playGenerator empties))
    RandomPlay {index, num} ->
      let empties = findEmpties model.board
          elem = indexList index empties
          loc = indexToLoc (indexList index empties)
          newBoard = placeVal loc num model.board
          newModel = {board=newBoard}
          isMove = log "move" (movesExist newBoard)
      in
          if not isMove then
            update Gameover newModel
          else
            (newModel, Cmd.none)
    Gameover -> update (Keystroke NewGame) initModel


canMoveLeft : Grid -> Bool
canMoveLeft g = left g /= g

canMoveRight : Grid -> Bool
canMoveRight g = right g /= g

canMoveUp : Grid -> Bool
canMoveUp g = up g /= g

canMoveDown : Grid -> Bool
canMoveDown g = down g /= g

movesExist : Grid -> Bool
movesExist g =
  if canMoveLeft g then True
  else if canMoveRight g then True
  else if canMoveUp g then True
  else if canMoveDown g then True
  else False

keyDecoder : Decode.Decoder Direction
keyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Direction
toDirection string =
  case string of
    "ArrowLeft" -> Left
    "ArrowRight" -> Right
    "ArrowUp" -> Up
    "ArrowDown" ->  Down
    " " -> NewGame --dumb shit
    _ -> Other

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
        [ Browser.Events.onKeyDown (Decode.map Keystroke keyDecoder)
        ]
   -- Browser.Events.onKeyDown (Decode.map (\key -> Tick) keyDecoder)

view : Model -> Html Msg
view model =
  let
    rowViews = List.map viewRow model.board
  in
     Html.div [Html.Attributes.class "Container"]
      [ Html.div [Html.Attributes.class "Board"] rowViews ]

viewRow : List Num -> Html Msg
viewRow row =
  let
    rowView = List.map viewCell row
  in
      Html.div [Html.Attributes.class "Row"] rowView

viewCell : Num -> Html Msg
viewCell num = Html.div [Html.Attributes.class ("Cell TileNum" ++ Debug.toString num)] [Html.text (String.fromInt num)]

-----------------------------------------------------
-- Other Helpers
-----------------------------------------------------

-- 0 = empty
emptyBoard : Grid
emptyBoard = List.repeat 4 [0,0,0,0]

locToIndex : Loc -> Index
locToIndex loc = loc.row * 4 + loc.col

indexToLoc : Index -> Loc
indexToLoc i =
  let row = i // 4
      col = i - (row*4)
  in Loc row col

findEmptyIndices : List Num -> Index -> List Index
findEmptyIndices nums index =
  case nums of
    [] -> []
    (i::rest) ->
      if i == 0 then
        index :: findEmptyIndices rest (index+1)
      else
        findEmptyIndices rest (index+1)


findEmpties : Grid -> List Index
findEmpties board =
  let flatBoard = List.concat board
  in findEmptyIndices flatBoard 0


playGenerator : List Index -> Generator Play
playGenerator empties =
  let n = List.length empties
  in Random.map2 Play (Random.int 0 (n-1)) (Random.weighted (75, 2) [(25,4)])


placeVal : Loc -> Num -> Grid -> Grid
placeVal loc num board =
  let flatInserted = List.Extra.setAt (locToIndex loc) num (flatten board)
  in unFlatten flatInserted

flatten : Grid -> List Num
flatten board = List.concat board

unFlatten : List Num -> Grid
unFlatten flatBoard = List.Extra.groupsOf 4 flatBoard

combine : List Num -> List Num
combine row =
  case row of
    [] -> []
    (x::y::rest) -> if x==y then (2*x) :: combine rest else x::(combine (y::rest))
    (x::rest) -> x :: combine rest

reflect ls = List.map List.reverse ls

shift ls =
  let nonzero = List.filter (\x -> x > 0) ls
      combined = combine nonzero
      numZeroes = 4 - List.length combined
      zeroes = List.repeat numZeroes 0
  in combined ++ zeroes

left ls  = List.map shift ls
right ls = (reflect << left << reflect) ls
up ls    = (List.Extra.transpose << left << List.Extra.transpose) ls
down ls  = (List.Extra.transpose << right << List.Extra.transpose) ls
