port module Elm_2048 exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Events
import Html.Attributes exposing (..)
import Random exposing (Generator)
import Time
import Debug
import Html.Attributes
import Array
import Json.Decode as Decode
import List.Extra

port gameoverPopup : () -> Cmd msg
port closePopup : (()->msg) -> Sub msg

-----------------------------------------------------
-- Types and Aliases
-----------------------------------------------------

type Meta = NewGame | Undo | OtherMeta

type Direction = Left | Right | Up | Down | OtherDir

type Msg = Tick | RandomPlay Play | Keystroke Direction | Gameover | Button Meta | ClosePopup


type alias Model = { board : Grid, score : Score, history : List (Grid, Score) }

type alias Grid = List (List Num)

type alias Play = {index : Index, num : Num}

type alias Loc = { row:Int, col:Int } -- top left = {0,0}

type alias Num = Int
type alias Index = Int
type alias Score = Int

type alias Flags = Int

-----------------------------------------------------
-- For New Game (random with seed)
-----------------------------------------------------

genTwoOrFour : Random.Seed -> (Int, Random.Seed)
genTwoOrFour seed = Random.step (Random.weighted (75, 2) [(25,4)]) seed

pickEmptySquare : List Index -> Random.Seed -> (Loc, Random.Seed)
pickEmptySquare locs seed =
  let n = List.length locs
      (index, newSeed) = Random.step (Random.int 0 (n-1)) seed
  in (indexToLoc (indexList index locs), newSeed)

getSeed : List Num -> Index -> Int -> Int
getSeed boardList index counter =
  case boardList of
    [] -> counter
    (x::xs) -> getSeed xs (index+1) (counter + x * (index * 7))
-----------------------------------------------------
-- Logging
-----------------------------------------------------
log : String -> a -> a
log s x =
  let stringX = Debug.toString x
  in case (Debug.log s stringX) of
    _ -> x
-----------------------------------------------------
-- List/Grid Functions
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


-- find elem at index i
indexList : Index -> List Num -> Num
indexList i ls =
  case (i, ls) of
    (_, [])      -> -1
    (0, l::_)    -> l
    (_, _::rest) -> indexList (i-1) rest


flatten : Grid -> List Num
flatten board = List.concat board

unFlatten : List Num -> Grid
unFlatten flatBoard = List.Extra.groupsOf 4 flatBoard

playGenerator : List Index -> Generator Play
playGenerator empties =
  let n = List.length empties
  in Random.map2 Play (Random.int 0 (n-1)) (Random.weighted (75, 2) [(25,4)])


placeVal : Loc -> Num -> Grid -> Grid
placeVal loc num board =
  let flatInserted = List.Extra.setAt (locToIndex loc) num (flatten board)
  in unFlatten flatInserted

scoreRow : List Num -> Score -> Score
scoreRow row score =
  case row of
    [] -> score
    (x::y::rest) -> if x==y then (2*x) + scoreRow rest score else scoreRow (y::rest) score
    (x::rest) -> scoreRow rest score

scoreHorizontal : Score -> Grid -> Score
scoreHorizontal score ls =
  let nonZero = List.map (List.filter (\x -> x > 0)) ls
  in List.foldl scoreRow score nonZero

scoreVertical : Score -> Grid -> Score
scoreVertical score ls = scoreHorizontal score (List.Extra.transpose ls)


combine : List Num -> List Num
combine row =
  case row of
    [] -> []
    (x::y::rest) -> if x==y then (2*x) :: combine rest else x::(combine (y::rest))
    (x::rest) -> x :: combine rest

reflect : Grid -> Grid
reflect ls = List.map List.reverse ls

shift : List Num -> List Num
shift ls =
  let nonzero = List.filter (\x -> x > 0) ls
      combined = combine nonzero
      numZeroes = 4 - List.length combined
      zeroes = List.repeat numZeroes 0
  in combined ++ zeroes

left : Grid -> Grid
left ls = List.map shift ls

right : Grid -> Grid
right ls = (reflect << left << reflect) ls

up : Grid -> Grid
up ls = (List.Extra.transpose << left << List.Extra.transpose) ls

down : Grid -> Grid
down ls = (List.Extra.transpose << right << List.Extra.transpose) ls

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
init currentTime = (initModel currentTime, Cmd.none)

initModel : Flags -> Model
initModel currentTime =

  let seed = Random.initialSeed currentTime
      board = emptyBoard
      empties = findEmpties board
      (num1, seed1) = genTwoOrFour seed
      (loc1, seed2) = pickEmptySquare empties seed1
      board1 = placeVal loc1 num1 board

      empties1 = findEmpties board1
      (num2, seed3) = genTwoOrFour seed2
      (loc2, seed4) = pickEmptySquare empties1 seed3
      board2 = placeVal loc2 num2 board1
  in {board=board2, score=0, history = []}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Button meta ->
      case meta of
        NewGame ->
          let seed = getSeed (flatten model.board) 0 0
          in init seed
        Undo ->
          case model.history of
            [] -> (model, Cmd.none)
            (b,s)::rest -> ({model | board = b, score=s, history = rest}, Cmd.none)
        OtherMeta -> (model, Cmd.none)
    Keystroke dir ->
      case (log "dir" dir) of
        Left  ->
          if canMoveLeft model.board then
            let newBoard = left model.board
                newScore = scoreHorizontal model.score model.board
                newHistory = (model.board, model.score)::model.history
            in update Tick {model | board = newBoard, score=newScore, history = newHistory}
          else (model, Cmd.none)
        Right ->
          if canMoveRight model.board then
            let newBoard = right model.board
                newScore = scoreHorizontal model.score model.board
                newHistory = (model.board, model.score)::model.history
            in update Tick {model | board = newBoard, score=newScore, history = newHistory}
          else (model, Cmd.none)
        Up    ->
          if canMoveUp model.board then
            let newBoard = up model.board
                newScore = scoreVertical model.score model.board
                newHistory = (model.board, model.score)::model.history
            in update Tick {model | board = newBoard, score=newScore, history = newHistory}
          else (model, Cmd.none)
        Down  ->
          if canMoveDown model.board then
            let newBoard = down model.board
                newScore = scoreVertical model.score model.board
                newHistory = (model.board, model.score)::model.history
            in update Tick {model | board = newBoard, score=newScore, history = newHistory}
          else (model, Cmd.none)
        OtherDir -> (model, Cmd.none)
    Tick ->
      let empties = findEmpties model.board
      in (model, Random.generate RandomPlay (playGenerator empties))
    RandomPlay {index, num} ->
      let empties = findEmpties model.board
          loc = indexToLoc (indexList index empties)
          newBoard = placeVal loc num model.board
          newModel = {model | board = newBoard}
          isMove = log "move" (movesExist newBoard)
      in
          if not isMove then
            update Gameover newModel
          else
            (newModel, Cmd.none)
    Gameover -> ( model, gameoverPopup () )
    ClosePopup -> ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
        [ Browser.Events.onKeyDown (Decode.map Keystroke keyDecoder),
        closePopup (\x -> ClosePopup)]

view : Model -> Html Msg
view model =
  let
    rowViews = List.map viewRow model.board
    titleText = Html.text ("2048")
    subtitleText = Html.text ("by Alex & Jacob")
    scoreText = Html.text (Debug.toString model.score) --INPUT SCORE HERE
  in
     Html.div [Html.Attributes.class "Container"]
      [
      Html.div [Html.Attributes.class "title"] [titleText],
      Html.div [Html.Attributes.class "subtitle"] [subtitleText],
      Html.div [Html.Attributes.class "score"] [scoreText],
      Html.div [Html.Attributes.class "newgame-button"] [Html.button [Html.Events.onClick (Button NewGame)] [text "New Game"]],
      Html.div [Html.Attributes.class "undo-button"] [Html.button [Html.Events.onClick (Button Undo)] [text "Undo"]],
      Html.div [Html.Attributes.class "Board"] rowViews
      ]

viewRow : List Num -> Html Msg
viewRow row =
  let
    rowView = List.map viewCell row
  in
      Html.div [Html.Attributes.class "Row"] rowView

viewCell : Num -> Html Msg
viewCell num = Html.div [Html.Attributes.class ("Cell TileNum" ++ Debug.toString num)] [Html.text (String.fromInt num)]

-----------------------------------------------------
-- User Input
-----------------------------------------------------

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
    _ -> OtherDir
