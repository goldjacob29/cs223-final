module Elm_2048 exposing (..)

import Matrix

import Browser
import Browser.Events
import Html exposing (Html)
import Random exposing (Generator)
import Time
import Debug
import Html.Attributes
import Array
import Json.Decode as Decode

-----------------------------------------------------
-- Types and Aliases
-----------------------------------------------------

type alias Model =
  { board : Matrix.Matrix Int }

type Msg = Tick | RandomPlay Play

type alias Play =
  {index : Index, num : Num}

-- top left = {0,0}
type alias Loc = { row:Int, col:Int }

type alias Num = Int
type alias Index = Int

type alias Flags = ()
-----------------------------------------------------
-- List Functions
-----------------------------------------------------

myHead : List a -> a
myHead ls =
  case ls of
    (h::_) -> h
    _ -> Debug.todo "error"

myTail : List a -> List a
myTail ls =
  case ls of
    (_::t) -> t
    _ -> Debug.todo "error"

transpose : List (List a) -> List (List a)
transpose ls =
  case ls of
    ([]::_) -> []
    _ -> (List.map myHead ls) :: transpose (List.map myTail ls)

-- find elem at index i
indexList : Index -> List a -> a
indexList i ls =
  case (i, ls) of
    (_, [])      -> Debug.todo "error"
    (0, l::_)    -> l
    (_, _::rest) -> indexList (i-1) rest

-----------------------------------------------------
-- Other Shit
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

-- 0 = empty
emptyBoard : Matrix.Matrix Num
emptyBoard = Matrix.repeat 4 4 0

-- locToIndex : Loc -> Index
-- locToIndex loc = loc.row * 4 + loc.col

indexToLoc : Index -> Loc
indexToLoc i =
  let row = i // 4
      col = i - (row*4)
  in Loc row col

-- findEmptyIndices : List Num -> Index -> List Loc
findEmptyIndices nums index =
  case nums of
    [] -> []
    (i::rest) ->
      if i == 0 then
        index :: findEmptyIndices rest (index+1)
      else
        findEmptyIndices rest (index+1)

findEmpties : Matrix.Matrix Num -> List Index
findEmpties board =
  let boardList = Array.toList (Matrix.toArray board)
  in findEmptyIndices boardList 0


playGenerator : List Index -> Generator Play
playGenerator empties =
  let n = List.length empties
  in Random.map2 Play (Random.int 0 (n-1)) (Random.weighted (75, 2) [(25,4)])

placeVal : Loc -> Num -> Matrix.Matrix Num -> Matrix.Matrix Num
placeVal loc num board = Matrix.set loc.col loc.row num board

initModel : Model
initModel = {board=emptyBoard}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      let empties = findEmpties model.board
      in (model, Random.generate RandomPlay (playGenerator empties))
    RandomPlay {index, num} ->
      let empties = findEmpties model.board
          loc = indexToLoc (indexList index empties)
          newBoard = placeVal loc num model.board
          newModel = {board = newBoard}
      in (newModel, Cmd.none)


keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

subscriptions : Model -> Sub Msg
subscriptions model =
   Browser.Events.onKeyDown (Decode.map (\key -> Tick) keyDecoder)

renderList lst =
  Html.ul []
    (List.map (\l -> Html.li [] [ Html.text (String.fromInt l)]) lst)

view : Model -> Html Msg
view model =
  let boardList = Array.toList (Matrix.toArray model.board)
  in renderList boardList
