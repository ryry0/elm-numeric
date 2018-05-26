module Matrix exposing
  ( fromList
  , mul
  , sMul
  , prettyPrint
  , debugPrint
  , add
  , equivalent
  , from2DList
  , sDiv
  , cvecFromList
  , rvecFromList
  , cvec
  , rvec
  , vec
  , dot
  , mat
  , vcat
  , get
  , zeroes
  , ones
  )

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

{-| Create a (n x m) matrix with inner lists being rows.
The following is a 2 x 3 matrix:
  matrix = Matrix.from2DList [
    [2, 2, 2],
    [3, 3, 3]
  ]
-}
from2DList : List (List Float) -> Matrix
from2DList a =
  case List.head a of
    Just data ->
      let
        columns = List.length <| data
        rows = List.length a
        -- check if all sublists are same length
        is_correct = List.all ((==) columns) <| List.map List.length a
      in
      if is_correct then
        fromList (rows, columns) <| List.concat a
      else
        Err <| "One or more of the sublist rows are malformed"

    Nothing ->
        fromList (0, 0) []

{-| Create a (n x m) matrix with inner lists being rows.
The following is a 2 x 3 matrix:
  matrix = Matrix.from2DList [
    [2, 2, 2],
    [3, 3, 3]
  ]
-}
mat : List (List Float) ->  Matrix
mat = from2DList

{-| Create a column vector from a list
-}
cvecFromList : List Float -> Matrix
cvecFromList a =
  fromList (List.length a, 1) a

{-| Create a column vector from a list
-}
cvec : List Float -> Matrix
cvec = cvecFromList

{-| Create a column vector from a list
-}
vec : List Float -> Matrix
vec = cvec

{-| Create a row vector from a list
-}
rvecFromList : List Float -> Matrix
rvecFromList a =
  fromList (1, List.length a) a

{-| Create a row vector from a list
-}
rvec : List Float -> Matrix
rvec = cvecFromList

{-| Multiply two correctly formed matrices
-}
mulCorrect : Matnxn -> Matnxn ->  Matrix
mulCorrect a b =
    Mat a

{-| Multiply with error handling
-}
mul : Matrix -> Matrix -> Matrix
mul a b =
  case (a,b) of
    (Mat a_, Mat b_) ->
      if numColumns a_ /= numRows b_ then
         let
             acolumns = toString <| numColumns a_
             brows = toString <| numRows b_
         in
         Err <| "Dimension mismatch in a*b: a.columns = " ++ acolumns ++ "b.rows = " ++ brows ++ "."
      else
        mulCorrect a_ b_

    (_, _) ->
      forwardError "[function mul]" a b

prettyPrint : Matrix -> String
prettyPrint a =
  case a of
    Mat mat ->
      prettyPrintCorrect mat

    Err string ->
      string

prettyPrintCorrect : Matnxn -> String
prettyPrintCorrect a =
  let
      strings = List.map ((++) " ") <| List.map toString a.elements
      structured_strings = make2D (numColumns a) strings
  in
     List.intersperse ["\n"] structured_strings
     |> List.concat
     |> List.foldr (++) ""

make2D : Int -> List a -> List (List a)
make2D num_row_elem list =
  case list of
    [] ->
      []
    items ->
      if List.length list < num_row_elem then
         [list]
      else
        List.take num_row_elem list :: make2D num_row_elem (List.drop num_row_elem list)

--takeColumns : Int -> List a -> List (List a)


{-| Get an item at index (row, column)
-}
get : (Int, Int) -> Matrix -> Maybe Float
get (r_index, c_index) a =
  case a of
    Mat a_ ->
      let
          check_r_bounds = 0 < r_index && r_index <= numRows a_
          check_c_bounds = 0 < c_index && c_index <= numColumns a_
      in
      if  check_r_bounds && check_c_bounds then
        List.head <| List.drop ( (r_index - 1) * (numColumns a_) + c_index - 1 )
          a_.elements
      else
        Nothing

    Err _ ->
      Nothing

{-| Add two matrices of identical dimensions together
-}
add : Matrix -> Matrix -> Matrix
add a b =
  case (a, b) of
    (Mat a_, Mat b_) ->
      if equalSize a_ b_ then
      fromList (numRows a_, numColumns a_) <| List.map2 (+) a_.elements b_.elements
      else
      let
          adims = dimToString a_
          bdims = dimToString b_
      in
        Err <| "Matrices not equal size. a: " ++ adims ++ ", b: " ++ bdims

    (_,_) ->
      forwardError "[function add]" a b

{-| Perform scalar multiplication on a matrix
-}
sMul : Float -> Matrix -> Matrix
sMul a b =
  case b of
    Mat b_ ->
      fromList (numRows b_, numColumns b_) <| List.map ((*) a) b_.elements

    Err string ->
      Err string

{-| Perform scalar division on a matrix
-}
sDiv : Float -> Matrix -> Matrix
sDiv a b =
  sMul (1/a) b

invert : Matrix -> Matrix
invert a =
  Err "Not implemented"

transpose : Matrix -> Matrix
transpose a =
  Err "Not implemented"

{-| Performs the dot product of two vectors
-}
dot : Matrix -> Matrix -> Maybe Float
dot a b =
  case (a,b) of
    (Mat a_, Mat b_) ->
    let
        a_is_vector = numColumns a_ == 1
        b_is_vector = numColumns b_ == 1
        same_length = (numRows a_) == (numRows b_)
    in
    if a_is_vector && b_is_vector && same_length then
       Just <| List.sum <| List.map2 (*) a_.elements b_.elements

    else
      Nothing

    (_,_) ->
      Nothing -- is there a way to do good scalar error handling?


cross : Matrix -> Matrix
cross a =
  Err "Not implemented"

{-| Checks if two matrices are of equal size
-}
equalSize : Matnxn -> Matnxn -> Bool
equalSize a b =
  (numRows a == numRows b) && (numColumns a == numColumns b)

{-| Checks if two matrices are equivalent
Should I make it epsilon based?
-}
equivalent : Matrix -> Matrix -> Bool
equivalent a b =
  case (a,b) of
    (Mat a_, Mat b_) ->
      let
          equal_size = equalSize a_ b_
          equal_members = List.all ((==) True) <| List.map2 (==) a_.elements b_.elements
      in
      if equal_size && equal_members then
         True
       else
         False

    _ ->
      False

{-| Helper to catch errors in functions of two variables (Matrix -> Matrix) ->
  Matrix
-}
forwardError : String -> Matrix -> Matrix -> Matrix
forwardError error a b =
  case (a,b) of
    (Err string, Mat _) ->
      Err <| error ++ "\n\t Matrix a: " ++ string

    (Mat _, Err string) ->
      Err <| error ++ "\n\t Matrix b: " ++ string

    (Err string, Err string2) ->
      Err <| error ++ "\n\t Matrix a: " ++ string ++ "Matrix b: " ++ string2

    (_, _) ->
      Err <| error ++ "\n\t Implement the correctly formed matrix branch."


{-| Helper to take a Matrix and stringify its dimensions
-}
dimToString : Matnxn -> String
dimToString a =
  let
        arows = toString <| numRows a
        acols = toString <| numColumns a
  in
  "(" ++ arows ++ "," ++ acols ++ ")"

{-| Concatenate two matrices vertically.
-}
vcat : Matrix -> Matrix -> Matrix
vcat a b =
  case (a, b) of
    (Mat a_, Mat b_) ->
    let
        acols = numColumns a_
        bcols = numColumns b_
        arows = numRows a_
        brows = numRows b_
    in
      if acols == bcols then
         fromList (arows + brows, acols) (List.append a_.elements b_.elements)
      else
        Err <| "Number of columns are not equal: a: " ++ toString acols ++ " b: " ++
          toString bcols

    (_, _) ->
      forwardError "[function vcat]" a b



{-| Helper to debug print
-}
debugPrint : Matrix -> String
debugPrint a =
  Debug.log (prettyPrint a) ""

-- Matrix generation utilities

{-| Generate a random matrix
-}
rand : (Int, Int) -> Matrix
rand a =
  Err "Not Implemented"

{-| Generate a matrix of ones
-}
ones : (Int, Int) -> Matrix
ones (rows, columns) =
  fromList (rows, columns) <| List.repeat  (rows*columns) 1


{-| Generate a matrix of zeroes
-}
zeroes : (Int, Int) -> Matrix
zeroes (rows, columns) =
  fromList (rows, columns) <| List.repeat  (rows*columns) 0

{-| Create an nxn identity matrix
-}
eye : Int -> Matrix
eye diagonal =
  case diagonal of
    0 ->
      from2DList []

    1 ->
      from2DList [[1]]

    2 ->
      from2DList [[1, 0], [0, 1]]

    _ ->
      Err "Not Implemented"
