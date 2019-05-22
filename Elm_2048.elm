module Elm_2048 exposing (..)

import Matrix

import Browser
import Html exposing (Html)
import Random exposing (Generator)
import Debug
import Html.Attributes

-- top left = {0,0}
type alias Loc = { row:Int, col:Int }

type alias Model =
  { board : Matrix.Matrix Int,
    seed : Random.Seed }

type alias Flags = ()

init : Flags -> Model
init () = initModel

-- 0 = empty
emptyBoard = Matrix.repeat 4 4 0

-- locGenerator : Generator Loc
-- locGenerator = Random.map2 Loc (Random.float 0 n)

locToNum loc = loc.row * 4 + loc.col
numToLoc num =
  let row = modBy 4 num
      col = num - (row*4)
  in Loc row col

nextLoc : Loc -> Loc
nextLoc loc = numToLoc ((locToNum loc) + 1)

indexList : Int -> List a -> a
indexList i ls =
  case (i, ls) of
    (_, []) -> Debug.todo "error"
    (0, l::_) -> l
    (_,_::rest) -> indexList (i-1) rest


-- findEmptySquares {0,0} mat
findEmptySquares : Loc -> Matrix.Matrix Int -> List Loc
findEmptySquares loc mat =
  if locToNum loc > 15 then [] else
  if Matrix.get loc.row loc.col mat == Result.Ok 0
    then loc :: findEmptySquares (nextLoc loc) mat
    else findEmptySquares (nextLoc loc) mat

pickEmptySquare : List Loc -> Random.Seed -> (Loc, Random.Seed)
pickEmptySquare locs seed =
  let n = List.length locs
      (index, newSeed) = Random.step (Random.int 0 (n-1)) seed
  in (indexList index locs, newSeed)

genTwoOrFour seed = Random.step (Random.weighted (75, 2) [(25,4)]) seed

placeOneVal : Loc -> Matrix.Matrix Int -> Random.Seed -> (Matrix.Matrix Int, Random.Seed)
placeOneVal loc mat seed =
  let (val,newSeed) = genTwoOrFour seed
  in (Matrix.set loc.row loc.col val mat, newSeed)

initModel : Model
initModel =
  let seed = Random.initialSeed 2
      b = emptyBoard
      empties = findEmptySquares (Loc 0 0) b
      (picked, seed1) = pickEmptySquare empties seed
      (b1, seed2) = placeOneVal picked b seed1

      empties1 = findEmptySquares (Loc 0 0) b1
      (picked1, seed3) = pickEmptySquare empties1 seed2
      (b2, seed4) = placeOneVal picked1 b1 seed3
  in {board=b2, seed=seed4}



-- transpose : List (List a) -> List (List a)
--
-- transpose ([]::_) = []
-- transpose x = (map head x) :: transpose (map tail x)
