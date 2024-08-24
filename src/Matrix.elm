module Matrix exposing
    ( Matrix
    , fromList, from2DList, mat, fromString, mats, zeroes, ones, eye, upper, lower, strictLower, strictUpper
    , cvecFromList, rvecFromList, cvec, rvec, vec
    , cross, dot
    , add, equivalent, sMul, sDiv, map, map2, eMul
    , mul, vcat, hcat, get, set, transpose, determinant, det, solveV, solve, invert, inv, luDecomp, getRows, getColumns, size
    , toString, debugPrint
    , to2DList, toFlatList
    )

{-| A matrix library written completely in Elm.
This library aims to be a reasonably complete suite of linear algebra tools.

Some highlights are that this library has generic sized matrices,
transposes, multiplication, and inversion.

    import Matrix as Mt


    --and program away!


# The Matrix Type

@docs Matrix


# Creating Matrices

@docs fromList, from2DList, mat, fromString, mats, zeroes, ones, eye, upper, lower, strictLower, strictUpper


# Creating Vectors

@docs cvecFromList, rvecFromList, cvec, rvec, vec


# Vector Operations

@docs cross, dot


# Matrix Element-wise Operations

@docs add, equivalent, sMul, sDiv, map, map2, eMul


# Matrix Operations

@docs mul, vcat, hcat, get, set, transpose, determinant, det, solveV, solve, invert, inv, luDecomp, getRows, getColumns, size


# Matrix Display

@docs toString, debugPrint


# Interop

@docs to2DList, toFlatList

-}

import Array exposing (Array)


type alias Matnxn =
    { dimensions : ( Int, Int )
    , elements : Array.Array Float
    }


{-| The Matrix type.
-}
type Matrix
    = Mat Matnxn



-- Matrix Creation


{-| Create a (n rows x m columns) matrix with the list as the elements.
Fails if dimension mismatch. Elements need to be specified in row-major order.

    matrix =
        Matrix.fromList ( 2, 3 ) [ 2, 2, 2, 3, 3, 3 ]

-}
fromList : ( Int, Int ) -> List Float -> Result String Matrix
fromList dimensions elements =
    fromArray dimensions <| Array.fromList elements


{-| Create a (n rows x m columns) matrix with the list as the elements.
Fails if dimension mismatch. Elements need to be specified in row-major order.
-}
fromArray : ( Int, Int ) -> Array.Array Float -> Result String Matrix
fromArray ( rows, columns ) elements =
    if rows * columns == Array.length elements then
        Ok (Mat { dimensions = ( rows, columns ), elements = elements })

    else
        let
            dimensions =
                String.fromInt (rows * columns)

            numelements =
                String.fromInt <| Array.length elements
        in
        Err <| "The dimensions, row * columns: " ++ dimensions ++ ", do not match the number of elements: " ++ numelements


{-| Create a (n x m) matrix with inner lists being rows.
The following is a 2 x 3 matrix:

    matrix =
        Matrix.from2DList
            [ [ 2, 2, 2 ]
            , [ 3, 3, 3 ]
            ]

-}
from2DList : List (List Float) -> Result String Matrix
from2DList a =
    case List.head a of
        Just data ->
            let
                columns =
                    List.length <| data

                rows =
                    List.length a

                -- check if all sublists are same length
                is_correct =
                    List.all ((==) columns) <| List.map List.length a
            in
            if is_correct then
                fromList ( rows, columns ) <| List.concat a

            else
                Err <| "One or more of the sublist rows are malformed"

        Nothing ->
            fromList ( 0, 0 ) []


{-| Create a (n x m) matrix with inner lists being rows.
The following is a 2 x 3 matrix:

    matrix =
        Matrix.mat
            [ [ 2, 2, 2 ]
            , [ 3, 3, 3 ]
            ]

-}
mat : List (List Float) -> Result String Matrix
mat =
    from2DList


{-| Create a (n x m) matrix with inner lists being rows.
In string format you can use Matlab/Julia-esque syntax.
Spaces denote elements in a row.
Semicolons denote elements in a column.
The string must begin with [ and end with ].

The following is a 2 x 3 matrix:

    matrix =
        Matrix.fromString "[ 2 2 2; 3 3 3]"

Any alpha/garbage characters will be set to zero.

-}
fromString : String -> Result String Matrix
fromString string =
    from2DList <| matParser string


{-| Shorthand for fromString
-}
mats : String -> Result String Matrix
mats =
    fromString


{-| Create a column vector from a list.

    column =
        Matrix.cvecFromList [ 1, 2, 3, 4 ]

-}
cvecFromList : List Float -> Matrix
cvecFromList a =
    let
        array : Array Float
        array =
            Array.fromList a
    in
    Mat
        { dimensions = ( Array.length array, 1 )
        , elements = array
        }


{-| Create a column vector from a list.
-}
cvec : List Float -> Matrix
cvec =
    cvecFromList


{-| Create a column vector from a list.
-}
vec : List Float -> Matrix
vec =
    cvec


{-| Create a row vector from a list.
-}
rvecFromList : List Float -> Matrix
rvecFromList a =
    let
        array : Array Float
        array =
            Array.fromList a
    in
    Mat
        { dimensions = ( 1, Array.length array )
        , elements = array
        }


{-| Create a row vector from a list.
-}
rvec : List Float -> Matrix
rvec =
    rvecFromList


{-| Generate a matrix of ones.

    lots_of_ones =
        Matrix.ones ( 4, 3 )

-}
ones : ( Int, Int ) -> Matrix
ones ( rows, columns ) =
    Mat
        { dimensions = ( rows, columns )
        , elements = Array.repeat (rows * columns) 1.0
        }


{-| Generate a matrix of zeroes.

    lots_of_zeroes =
        Matrix.zeroes ( 3, 4 )

-}
zeroes : ( Int, Int ) -> Matrix
zeroes ( rows, columns ) =
    Mat
        { dimensions = ( rows, columns )
        , elements = Array.repeat (rows * columns) 1.0
        }


{-| Create an nxn identity matrix.

    identity =
        Matrix.eye 3

-}
eye : Int -> Matrix
eye diagonal =
    let
        gen : Int -> Float
        gen x =
            if modBy (diagonal + 1) x == 0 then
                1

            else
                0
    in
    Mat
        { dimensions = ( diagonal, diagonal )
        , elements = Array.initialize (diagonal * diagonal) gen
        }


{-| Create an nxn upper triangular matrix.

    triangle =
        Matrix.upper 4

-}
upper : Int -> Matrix
upper diagonal =
    let
        range : List Int
        range =
            List.range 0 (diagonal - 1)

        f : Int -> List Float
        f i =
            List.append (List.repeat i 0.0) (List.repeat (diagonal - i) 1.0)

        list : List Float
        list =
            List.concatMap f range
    in
    Mat
        { dimensions = ( diagonal, diagonal )
        , elements = Array.fromList list
        }


{-| Create an nxn strict upper triangular matrix.
This means that elements along the diagonal are zero.

    striangle =
        Matrix.strictUpper 4

-}
strictUpper : Int -> Matrix
strictUpper diagonal =
    let
        range : List Int
        range =
            List.range 0 (diagonal - 1)

        f : Int -> List Float
        f i =
            List.append (List.repeat (i + 1) 0.0) (List.repeat (diagonal - i - 1) 1.0)

        list : List Float
        list =
            List.concatMap f range
    in
    Mat
        { dimensions = ( diagonal, diagonal )
        , elements = Array.fromList list
        }


{-| Create an nxn lower triangular matrix.

    ltriangle =
        Matrix.lower 4

-}
lower : Int -> Matrix
lower diagonal =
    transpose <| upper diagonal


{-| Create an nxn strict lower triangular matrix.
This means that elements along the diagonal are zero.

    sltriangle =
        Matrix.strictLower 4

-}
strictLower : Int -> Matrix
strictLower diagonal =
    transpose <| strictUpper diagonal



-- Operations


{-| Perform matrix multiplication

    A * B

    a =
        Matrix.eye (2, 2)

    b =
        Matrix.mats "[2, 3; 4 5]"

    c =
        mul a b

-}
mul : Matrix -> Matrix -> Result String Matrix
mul a b =
    if numColumns a /= numRows b then
        let
            acolumns =
                String.fromInt <| numColumns a

            brows =
                String.fromInt <| numRows b
        in
        Err <| "Dimension mismatch in a*b: a.columns = " ++ acolumns ++ " b.rows = " ++ brows ++ "."

    else
        let
            a_list =
                to2DList a

            b_list =
                to2DList <| transpose b

            --collapse a row from the left and column from the right into scalar
            collapse x y =
                List.sum <| List.map2 (*) x y

            --element level
            constructList x y =
                case x of
                    [] ->
                        []

                    m :: ms ->
                        List.map (collapse m) y :: constructList ms y
        in
        from2DList <| constructList a_list b_list


{-| Get the lu decomposition of a matrix
Since pivoting isn't implemented, watch out for numerical instability
-}
luDecomp : Matrix -> Result String ( Matrix, Matrix )
luDecomp a =
    if numRows a == numColumns a then
        luSplit a

    else
        Err "Must be a square matrix"


{-| Splits the lu factorized single matrix into two
-}
luSplit : Matrix -> Result String ( Matrix, Matrix )
luSplit a =
    let
        single =
            luNoPivotSingle a

        dim =
            numColumns a

        l =
            eMul single (strictLower dim)
                |> Result.andThen (add (eye dim))

        u =
            eMul single (upper dim)
    in
    Result.map2 Tuple.pair l u


{-| Performs lu factorization
-}
luNoPivotSingle : Matrix -> Matrix
luNoPivotSingle ((Mat a) as a_) =
    let
        lu =
            zeroes a.dimensions

        --i is inner loop j outer loop
        -- bind computelem to a
        luCompute index dest =
            luComputeElem index a_ dest

        indices =
            genIndices a.dimensions
    in
    List.foldl luCompute lu indices


genIndices : ( Int, Int ) -> List ( Int, Int )
genIndices ( i_range, j_range ) =
    let
        listj =
            List.concat <| List.map (List.repeat i_range) <| List.range 1 j_range

        listi =
            List.concat <| List.repeat j_range (List.range 1 i_range)
    in
    List.map2 (\a b -> ( a, b )) listi listj


luComputeElem : ( Int, Int ) -> Matrix -> Matrix -> Matrix
luComputeElem ( i, j ) original lu =
    let
        tiny =
            10 ^ -40

        getWithDefault index m =
            Maybe.withDefault 0.0 (get index m)

        aij =
            getWithDefault ( i, j ) original

        bjj =
            Maybe.withDefault tiny <| get ( j, j ) lu

        compute index =
            getWithDefault ( i, index ) lu * getWithDefault ( index, j ) lu
    in
    if i > j then
        let
            k =
                List.range 1 (j - 1)
        in
        -- NRIC 2.3.13
        set ( i, j ) ((aij - (List.sum <| List.map compute k)) / bjj) lu

    else
        let
            k =
                List.range 1 (i - 1)
        in
        -- NRIC 2.3.12
        set ( i, j ) (aij - (List.sum <| List.map compute k)) lu


getScaling : List (List Float) -> List Float
getScaling m =
    List.map (\x -> List.map abs x) m
        |> List.map List.maximum
        |> List.map (Maybe.withDefault 0)
        |> List.map (\x -> 1 / x)


{-| Get an item at index (row, column). Indices are 1-indexed.

    a =
        Matrix.mats "[3 4; 5 6]"

    b =
        -- equals 4
        get ( 1, 2 ) a

-}
get : ( Int, Int ) -> Matrix -> Maybe Float
get ( r_index, c_index ) (Mat a) =
    let
        ( arows, acols ) =
            a.dimensions

        check_r_bounds =
            0 < r_index && r_index <= arows

        check_c_bounds =
            0 < c_index && c_index <= acols
    in
    if check_r_bounds && check_c_bounds then
        Array.get ((r_index - 1) * acols + c_index - 1)
            a.elements

    else
        Nothing


{-| Get an item at index (row, column). Indices are 1-indexed.
-}
set : ( Int, Int ) -> Float -> Matrix -> Matrix
set ( r_index, c_index ) data (Mat a) =
    let
        ( arows, acols ) =
            a.dimensions

        check_r_bounds =
            0 < r_index && r_index <= arows

        check_c_bounds =
            0 < c_index && c_index <= acols
    in
    if check_r_bounds && check_c_bounds then
        { a
            | elements =
                Array.set ((r_index - 1) * acols + c_index - 1)
                    data
                    a.elements
        }
            |> Mat

    else
        Mat a


{-| Add two matrices of identical dimensions together

    a =
        Matrix.add (Matrix.zeroes ( 2, 2 )) (Matrix.ones ( 2, 2 ))

-}
add : Matrix -> Matrix -> Result String Matrix
add (Mat a) (Mat b) =
    if a.dimensions == b.dimensions then
        Mat
            { dimensions = a.dimensions
            , elements = arraymap2 (+) a.elements b.elements
            }
            |> Ok

    else
        let
            adims =
                dimToString a

            bdims =
                dimToString b
        in
        Err <| "Matrices not equal size. a: " ++ adims ++ ", b: " ++ bdims


{-| Map a function over all elements individually
-}
map : (Float -> Float) -> Matrix -> Matrix
map f (Mat a) =
    Mat
        { dimensions = a.dimensions
        , elements = Array.map f a.elements
        }


{-| Map a function over elements of same index between matrices
-}
map2 : (Float -> Float -> Float) -> Matrix -> Matrix -> Result String Matrix
map2 f (Mat a) (Mat b) =
    if a.dimensions == b.dimensions then
        fromArray a.dimensions <| arraymap2 f a.elements b.elements

    else
        Err "Unequal Matrix sizes"


{-| Perform scalar multiplication on a matrix.
-}
sMul : Float -> Matrix -> Matrix
sMul a b =
    map ((*) a) b


{-| Perform element by element multiplication on a matrix.
-}
eMul : Matrix -> Matrix -> Result String Matrix
eMul a b =
    map2 (*) a b


{-| Perform scalar division on a matrix.
-}
sDiv : Float -> Matrix -> Matrix
sDiv a b =
    sMul (1 / a) b


{-| Invert a square matrix.

    a =
        "[ 2 5; 6 7]"

    inva =
        invert a

    identity =
        mul a inva

-}
invert : Matrix -> Result String Matrix
invert a =
    solve a (eye (numRows a))


{-| Shorthand for invert.
-}
inv : Matrix -> Result String Matrix
inv =
    invert


{-| Solve a system of equations of the form
AX = B. You provide A and b and get back x
Where A, B, X are matrices
B is a matrix of solution vectors horizontally concatenated.

    a =
        Matrix.eye 3

    b =
        Matrix.hcat (Matrix.vec [ 3, 2, 1 ]) (Matrix.vec [ 1, 2, 3 ])

    x =
        Matrix.solve a b

-}
solve : Matrix -> Matrix -> Result String Matrix
solve a b =
    if numRows a == numRows b then
        let
            bs =
                getColumns b

            single =
                luNoPivotSingle a
        in
        List.foldr
            (\e acc ->
                case acc of
                    Ok acc_ ->
                        hcat e acc_

                    Err _ ->
                        acc
            )
            (Mat
                { dimensions = ( numRows a, 0 )
                , elements = Array.empty
                }
                |> Ok
            )
            (List.map (applySubstitution single) bs)

    else
        Err "Dimension mismatch"


{-| Solve a system of equations of the form
Ax = b. You provide A and b and get back x
Where A is a matrix, and b and x are vectors

    a =
        Matrix.eye 3

    b =
        Matrix.vec [ 3, 2, 1 ]

    x =
        Matrix.solve a b

-}
solveV : Matrix -> Matrix -> Result String Matrix
solveV a b =
    let
        b_is_vector =
            numColumns b == 1

        dimensions_match =
            numRows b == numColumns a

        a_is_square =
            numColumns a == numRows a
    in
    case ( b_is_vector, dimensions_match, a_is_square ) of
        ( True, True, True ) ->
            Ok (applySubstitution (luNoPivotSingle a) b)

        ( False, _, _ ) ->
            Err "b is not a vector."

        ( _, False, _ ) ->
            Err "Dimensions of A and b do not match"

        ( _, _, False ) ->
            Err "A is not square"


{-| Applies forward and backward substitution, decoupling substitution from
computing lu decomp
-}
applySubstitution : Matrix -> Matrix -> Matrix
applySubstitution single b =
    let
        brows =
            numRows b

        z =
            zeroes ( brows, 1 )
    in
    let
        is =
            List.range 1 (numRows b)

        fsBound i y_ =
            forwardSubstitution i single b y_

        y =
            List.foldl fsBound z is

        bsBound i x_ =
            backSubstitution i single y x_
    in
    List.foldr bsBound z is


{-| Perform forward substitution remembering that the diagonal of the lower lu
matrix is all ones.
l is technically the lower lu matrix
b is the original passed in vector of b's
y is the solution, updated piecewise, of L \* y = b

Numerical Recipes in C 2.3.6

-}
forwardSubstitution : Int -> Matrix -> Matrix -> Matrix -> Matrix
forwardSubstitution i l b y =
    let
        getWithDefault index m =
            Maybe.withDefault 0.0 (get index m)

        getVecDefault index v =
            getWithDefault ( index, 1 ) v

        yi =
            case i of
                1 ->
                    getVecDefault i b

                _ ->
                    let
                        bi =
                            getVecDefault i b

                        j =
                            List.range 1 (i - 1)

                        lijs =
                            List.map (\curr_j -> getWithDefault ( i, curr_j ) l) j

                        yjs =
                            List.map (\curr_j -> getVecDefault curr_j y) j

                        sum =
                            List.sum <| List.map2 (*) lijs yjs
                    in
                    bi - sum
    in
    set ( i, 1 ) yi y


{-| Perform forward substitution remembering that the diagonal of the lower lu
matrix is all ones.
u is technically the upper lu matrix
y is the original passed in vector of b's
x is the solution, updated piecewise of BOTH Ux = y and the original Ax = b

Numerical Recipes in C 2.3.7

-}
backSubstitution : Int -> Matrix -> Matrix -> Matrix -> Matrix
backSubstitution i u y x =
    let
        getWithDefault index m =
            Maybe.withDefault 0.0 (get index m)

        getVecDefault index v =
            getWithDefault ( index, 1 ) v

        uii =
            getWithDefault ( i, i ) u

        xrows =
            numRows x

        xi =
            if i == xrows then
                getVecDefault i y / uii

            else
                let
                    --aij = getWithDefault ( i, j ) original
                    yi =
                        getVecDefault i y

                    j =
                        List.range (i + 1) xrows

                    uijs =
                        List.map (\curr_j -> getWithDefault ( i, curr_j ) u) j

                    xjs =
                        List.map (\curr_j -> getVecDefault curr_j x) j

                    sum =
                        List.sum <| List.map2 (*) uijs xjs
                in
                (yi - sum) / uii
    in
    set ( i, 1 ) xi x


{-| Transpose a matrix.
-}
transpose : Matrix -> Matrix
transpose ((Mat a) as m) =
    let
        ( arows, acols ) =
            a.dimensions

        f : Int -> Float
        f index =
            let
                mappedIndex : Int
                mappedIndex =
                    modBy arows index
                        * numColumns m
                        + (index // arows)
            in
            Maybe.withDefault 0.0 (Array.get mappedIndex a.elements)
    in
    Mat
        { dimensions = ( acols, arows )
        , elements = Array.initialize (acols * arows) f
        }


{-| Get the determinant of a square matrix.

    a =
        Matrix.mats "[1 2 3; 4 5 6; 7 8 9]"

    is_singular =
        if determinant a == 0 then
            "Matrix is singular"

        else
            "Matrix is not singular"

-}
determinant : Matrix -> Maybe Float
determinant a =
    if numRows a == numColumns a then
        let
            single =
                luNoPivotSingle a
        in
        List.range 1 (numRows a)
            |> List.map (\x -> ( x, x ))
            |> List.map (\x -> Maybe.withDefault 0.0 (get x single))
            |> List.product
            |> Just

    else
        Nothing


{-| Shorthand for determinant.
-}
det : Matrix -> Maybe Float
det =
    determinant


{-| Performs the dot product of two nxn vectors

    x =
        Matrix.vec [ 1, 0, 0 ]

    y =
        Matrix.vec [ 0, 1, 0 ]

    zero =
        Matrix.dot x y

-}
dot : Matrix -> Matrix -> Maybe Float
dot (Mat a) (Mat b) =
    let
        ( arows, acols ) =
            a.dimensions

        ( brows, bcols ) =
            b.dimensions

        a_is_vector =
            acols == 1

        b_is_vector =
            bcols == 1

        same_length =
            arows == brows
    in
    if a_is_vector && b_is_vector && same_length then
        arraymap2 (*) a.elements b.elements
            |> Array.foldr (+) 0
            |> Just

    else
        Nothing



-- is there a way to do good scalar error handling?


{-| Get the cross product of two 3d vectors. a >< b

    x =
        Matrix.vec [ 1, 0, 0 ]

    y =
        Matrix.vec [ 0, 1, 0 ]

    z =
        Matrix.cross x y

-}
cross : Matrix -> Matrix -> Result String Matrix
cross a b =
    if check3dVec a && check3dVec b then
        let
            getDefault x v =
                Maybe.withDefault 0.0 <| get x v

            ax =
                getDefault ( 1, 1 ) a

            ay =
                getDefault ( 2, 1 ) a

            az =
                getDefault ( 3, 1 ) a

            bx =
                getDefault ( 1, 1 ) b

            by =
                getDefault ( 2, 1 ) b

            bz =
                getDefault ( 3, 1 ) b
        in
        let
            i =
                (ay * bz) - (by * az)

            j =
                -1 * ((ax * bz) - (bx * az))

            k =
                (ax * by) - (bx * ay)
        in
        Ok (vec <| [ i, j, k ])

    else
        Err "One or both vectors are malformed."


{-| Checks if two matrices are of equal size
-}
equalSize : Matrix -> Matrix -> Bool
equalSize (Mat a) (Mat b) =
    a.dimensions == b.dimensions


{-| Checks if two matrices are equivalent within some epsilon.

    epsilon =
        10 ^ -4

    is_equivalent =
        equivalent epsilon a b

-}
equivalent : Float -> Matrix -> Matrix -> Bool
equivalent epsilon (Mat a) (Mat b) =
    (a.dimensions == b.dimensions)
        && (arraymap2 (-) a.elements b.elements
                |> Array.map (\x -> abs x < epsilon)
                |> Array.toList
                |> List.all ((==) True)
           )


{-| Concatenate two matrices vertically.
-}
vcat : Matrix -> Matrix -> Result String Matrix
vcat (Mat a) (Mat b) =
    let
        ( arows, acols ) =
            a.dimensions

        ( brows, bcols ) =
            b.dimensions
    in
    if acols == bcols then
        Mat
            { dimensions = ( arows + brows, acols )
            , elements = Array.append a.elements b.elements
            }
            |> Ok

    else
        Err <|
            "Number of columns are not equal: a: "
                ++ String.fromInt acols
                ++ " b: "
                ++ String.fromInt bcols


{-| Concatenate two matrices horizontally.
-}
hcat : Matrix -> Matrix -> Result String Matrix
hcat a b =
    let
        arows =
            numRows a

        brows =
            numRows b
    in
    if arows == brows then
        vcat (transpose a) (transpose b)
            |> Result.map transpose

    else
        Err <|
            "Number of rows are not equal: a: "
                ++ String.fromInt arows
                ++ " b: "
                ++ String.fromInt brows


{-| Returns matrix as flat list

    Matrix.toFlatList (Matrix.eye 2) == [ 1, 0, 0, 1 ]

-}
toFlatList : Matrix -> List Float
toFlatList (Mat n) =
    Array.toList n.elements


{-| Returns matrix as 2d list.
Returns empty list if Matrix is in error.

    Matrix.to2DList (Matrix.eye 2) == [ [ 1, 0 ], [ 0, 1 ] ]

-}
to2DList : Matrix -> List (List Float)
to2DList (Mat z) =
    let
        ( _, zcols ) =
            z.dimensions
    in
    make2D zcols (Array.toList z.elements)


{-| Returns size of matrix, (rows, columns)
-}
size : Matrix -> ( Int, Int )
size (Mat { dimensions }) =
    dimensions



-- Auxiliary Functions


{-| Helper to take a Matrix and stringify its dimensions
-}
dimToString : Matnxn -> String
dimToString a =
    let
        ( arows, acols ) =
            a.dimensions
    in
    "(" ++ String.fromInt arows ++ "," ++ String.fromInt acols ++ ")"


{-| Change matrix into string form, such as what would be displayed in the terminal.

    Matrix.toString (Matrix.eye 3) == " 1 0 0\n 0 1 0\n 0 0 1"

-}
toString : Matrix -> String
toString (Mat a) =
    let
        ( _, columns ) =
            a.dimensions

        strings =
            a.elements
                |> Array.toList
                |> List.map String.fromFloat
                |> List.map ((++) " ")

        structured_strings =
            make2D columns strings

        matrix_string =
            structured_strings
                |> List.intersperse [ "\n" ]
                |> List.concat
                |> List.foldr (++) ""
    in
    matrix_string


{-| Helper to re-2dify a flat matrix
-}
make2D : Int -> List a -> List (List a)
make2D num_row_elem list =
    case list of
        [] ->
            []

        _ ->
            if List.length list < num_row_elem then
                [ list ]

            else
                List.take num_row_elem list :: make2D num_row_elem (List.drop num_row_elem list)


{-| Returns the rows of a matrix in a list.
-}
getRows : Matrix -> List Matrix
getRows a =
    List.map rvec <| to2DList a


{-| Returns the columns of a matrix in a list.
-}
getColumns : Matrix -> List Matrix
getColumns a =
    List.map vec <| to2DList (transpose a)


{-| Helper to debug print. Most useful in repl.

    > Matrix.debugPrint (Matrix.eye 3)
    (3, 3) Matrix
     1 0 0
     0 1 0
     0 0 1
     : ""
     ""

-}
debugPrint : Matrix -> String
debugPrint a =
    let
        dimensions : ( Int, Int )
        dimensions =
            size a

        num_rows : String
        num_rows =
            String.fromInt <| Tuple.first dimensions

        num_cols : String
        num_cols =
            String.fromInt <| Tuple.second dimensions

        description_string : String
        description_string =
            "(" ++ num_rows ++ "," ++ num_cols ++ ")" ++ " Matrix\n"

        final_string : String
        final_string =
            description_string ++ toString a ++ "\n"
    in
    final_string


{-| Get the number of rows in a matrix
-}
numRows : Matrix -> Int
numRows (Mat a) =
    Tuple.first a.dimensions


{-| Get the number of columns in a matrix
-}
numColumns : Matrix -> Int
numColumns (Mat a) =
    Tuple.second a.dimensions


{-| Check if the matrix is a 3d vector
-}
check3dVec : Matrix -> Bool
check3dVec (Mat a) =
    a.dimensions == ( 3, 1 )


{-| Map2 for arrays
-}
arraymap2 : (a -> b -> c) -> Array.Array a -> Array.Array b -> Array.Array c
arraymap2 f a b =
    if Array.isEmpty a || Array.isEmpty b then
        Array.fromList []

    else
        let
            dropArr aa =
                Array.slice 1 (Array.length aa) aa

            result =
                case ( Array.get 0 a, Array.get 0 b ) of
                    ( Just aval, Just bval ) ->
                        Array.fromList [ f aval bval ]

                    _ ->
                        Array.fromList []
        in
        Array.append result (arraymap2 f (dropArr a) (dropArr b))



--string must be enclosed by brackets


matParser : String -> List (List Float)
matParser string =
    if String.startsWith "[" string && String.endsWith "]" string then
        String.dropLeft 1 string
            |> String.dropRight 1
            |> String.split ";"
            |> List.map (String.split " ")
            |> List.map (List.filter (\x -> not <| String.isEmpty x))
            |> List.map (List.map (\x -> Maybe.withDefault 0 (String.toFloat x)))

    else
        []
