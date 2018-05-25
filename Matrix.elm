module Matrix exposing (fromList, mul, sMul, prettyPrint, add)

type alias Matnxn =
  {
    dimensions : (Int, Int),
    elements : List Float
  }


type Matrix
  = Mat Matnxn
  | Err String

{-| Get the number of rows in a matrix
-}
numRows : Matnxn -> Int
numRows a =
  Tuple.first a.dimensions

{-| Get the number of columns in a matrix
-}
numColumns : Matnxn -> Int
numColumns a =
  Tuple.second a.dimensions

{-| Create a (n x m) matrix with the list as the elements.
Fails if dimension mismatch. Elements need to be specified in row-major order.
-}
fromList : (Int, Int) -> List Float -> Matrix
fromList (rows, columns) elements =
  if rows * columns == List.length elements then
     Mat { dimensions = (rows, columns), elements = elements }
  else
  let
      dimensions = toString (rows * columns)
      numelements = toString <| List.length elements
  in
    Err <| "The dimensions, row * columns: " ++ dimensions ++ ", do not match the number of elements: " ++ numelements

from2dList : List (List Float) -> Matrix
from2dList a =
  Err "Not implemented"

mulCorrect : Matnxn -> Matnxn ->  Matrix
mulCorrect a b =
    Mat a

mul : Matrix -> Matrix -> Matrix
mul a b =
  case (a,b) of
    (Mat _, Err string) ->
      Err <| "Matrix a: " ++ string

    (Err string, Mat _) ->
      Err <| "Matrix b: " ++ string

    (Err string, Err string2) ->
      Err <| "Matrix a: " ++ string ++ "Matrix b: " ++ string2

    (Mat a_, Mat b_) ->
      if numColumns a_ /= numRows b_ then
         let
             acolumns = toString <| numColumns a_
             brows = toString <| numRows b_
         in
         Err <| "Dimension mismatch in a*b: a.columns = " ++ acolumns ++ "b.rows = " ++ brows ++ "."
      else
        mulCorrect a_ b_

prettyPrint : Matrix -> String
prettyPrint a =
  case a of
    Err string ->
      string
    Mat mat ->
      prettyPrintCorrect mat

prettyPrintCorrect : Matnxn -> String
prettyPrintCorrect a =
  let
      strings = List.map ((++) " ") <| List.map toString a.elements
      structured_strings = takeRows (numColumns a) strings
  in
     List.intersperse ["\n"] structured_strings
     |> List.concat
     |> List.foldr (++) ""

takeRows : Int -> List a -> List (List a)
takeRows numelem list =
  case list of
    [] ->
      []
    items ->
      if List.length list < numelem then
         [list]
      else
        List.take numelem list :: takeRows numelem (List.drop numelem list)

--takeColumns : Int -> List a -> List (List a)

eye : Int -> Matrix
eye diagonal =
  Err "Not implemented"

get : (Int, Int) -> Matrix -> Matrix
get (r_index, c_index) a =
  case a of
    Err string ->
      Err string

    Mat a_ ->
      Err "Not implemented"

{-| Add two matrices of identical dimensions together
-}
add : Matrix -> Matrix -> Matrix
add a b =
  case (a, b) of
    (Mat a_, Mat b_) ->
      if equalSize a_ b_ then
      fromList (numRows a_, numColumns a_) <| List.map2 (+) a_.elements b_.elements
      else
        Err "Matrices not equal size."

    (_,_) ->
      Err "Not implemented"

sMul : Float -> Matrix -> Matrix
sMul a b =
  case b of
    Mat b_ ->
      fromList (numRows b_, numColumns b_) <| List.map ((*) a) b_.elements

    Err string ->
      Err string

invert : Matrix -> Matrix
invert a =
  Err "Not implemented"

transpose : Matrix -> Matrix
transpose a =
  Err "Not implemented"

dot : Matrix -> Matrix -> Float
dot a b =
  0.0


cross : Matrix -> Matrix
cross a =
  Err "Not implemented"

{-| Checks if two matrices are of equal size
-}
equalSize : Matnxn -> Matnxn -> Bool
equalSize a b =
  (numRows a == numRows b) && (numColumns a == numColumns b)

{-| Checks if two matrices are equivalent
-}
equivalent : Matrix -> Matrix -> Bool
equivalent a b =
  case (a,b) of
    (Mat a_, Mat b_) ->
      False

    _ ->
      False

forwardError : Matrix -> Matrix -> Matrix
forwardError a b =
  Err "Not implemented"
