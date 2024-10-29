module Matrix exposing
    ( Matrix
    , fromList, from2DList, mat, fromString, mats, zeroes, ones, eye, upper, lower, strictLower, strictUpper, initialize
    , cvecFromList, rvecFromList, cvec, rvec, vec
    , cross, dot
    , add, equivalent, sMul, sDiv, map, map2, eMul
    , mul, vcat, hcat, get, set, transpose, determinant, det, solveV, solve, invert, inv, luDecomp, getRows, getColumns, size
    , toString, toAlignedString, debugPrint
    , to2DList, toFlatList
    , toFractionString
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

@docs fromList, from2DList, mat, fromString, mats, zeroes, ones, eye, upper, lower, strictLower, strictUpper, initialize


# Creating Vectors

@docs cvecFromList, rvecFromList, cvec, rvec, vec


# Vector Operations

@docs cross, dot


# Matrix Element-wise Operations

@docs add, equivalent, sMul, sDiv, map, map2, eMul


# Matrix Operations

@docs mul, vcat, hcat, get, set, transpose, determinant, det, solveV, solve, invert, inv, luDecomp, getRows, getColumns, size


# Matrix Display

@docs toString, toAlignedString, debugPrint


# Interop

@docs to2DList, toFlatList

-}

import Array exposing (Array)
import Array.Extra
import List.Extra
import Matrix.Format
import Maybe.Extra


type alias Matnxn =
    { dimensions : ( Int, Int )
    , elements : Array Float
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
fromArray : ( Int, Int ) -> Array Float -> Result String Matrix
fromArray ( rows, columns ) elements =
    if rows * columns == Array.length elements then
        Ok (Mat { dimensions = ( rows, columns ), elements = elements })

    else
        let
            dimensions : String
            dimensions =
                String.fromInt (rows * columns)

            numelements : String
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
                columns : Int
                columns =
                    List.length <| data

                -- check if all sublists are same length
                is_correct : Bool
                is_correct =
                    List.all ((==) columns) <| List.map List.length a
            in
            if is_correct then
                let
                    rows : Int
                    rows =
                        List.length a
                in
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
    initialize ( diagonal, diagonal )
        (\r c ->
            if r == c then
                1

            else
                0
        )


initialize : ( Int, Int ) -> (Int -> Int -> Float) -> Matrix
initialize ( rows, cols ) gen =
    Mat
        { dimensions = ( rows, cols )
        , elements = Array.initialize (rows * cols) (\i -> gen (i // cols) (modBy cols i))
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
            acols : String
            acols =
                String.fromInt <| numColumns a

            brows : String
            brows =
                String.fromInt <| numRows b
        in
        Err <| "Dimension mismatch in a*b: a.columns = " ++ acols ++ " b.rows = " ++ brows ++ "."

    else
        let
            a_list : List (List Float)
            a_list =
                to2DList a

            b_list : List (List Float)
            b_list =
                to2DList <| transpose b

            --collapse a row from the left and column from the right into scalar
            collapse : List number -> List number -> number
            collapse x y =
                List.sum <| List.map2 (*) x y

            --element level
            constructList : List (List number) -> List (List number) -> List (List number)
            constructList x y =
                case x of
                    [] ->
                        []

                    m :: ms ->
                        List.map (collapse m) y :: constructList ms y
        in
        from2DList <| constructList a_list b_list


{-| Get the PLU (LU with pivoting) decomposition of a matrix.

Given `A` returns `P`, `L`, `U` such that:

  - `A = P L U`,
  - `P` is a permutation matrix,
  - `L` is lower triangular with ones on the diagonal and
  - `U` is upper triangular.

-}
luDecomp : Matrix -> { p : Matrix, l : Matrix, u : Matrix }
luDecomp a =
    let
        rows : Int
        rows =
            numRows a

        cols : Int
        cols =
            numColumns a

        logMatrix : String -> List (List Float) -> ()
        logMatrix label m =
            Debug.log
                ("\n"
                    ++ label
                    ++ ":\n"
                    ++ toAlignedString (from2DListUnsafe m)
                )
                ()

        epsilon : Float
        epsilon =
            10 ^ -14

        from2DListUnsafe : List (List Float) -> Matrix
        from2DListUnsafe list =
            list
                |> from2DList
                |> Result.withDefault (eye 0)

        mulUnsafe : Matrix -> Matrix -> Matrix
        mulUnsafe x y =
            mul x y
                |> Result.withDefault (eye 0)

        go i prevP prevL prevU =
            let
                _ =
                    Debug.log "\n------------------ i" i
            in
            if i == min rows cols then
                let
                    _ =
                        logMatrix "lastP" prevP

                    _ =
                        logMatrix "lastL" prevL

                    _ =
                        logMatrix "lastU" prevU
                in
                { p = from2DListUnsafe prevP
                , l = from2DListUnsafe prevL
                , u = from2DListUnsafe prevU
                }

            else
                let
                    _ =
                        logMatrix "prevP" prevP

                    _ =
                        logMatrix "prevL" prevL

                    _ =
                        logMatrix "prevU" prevU
                in
                case findPivot epsilon i prevU of
                    Nothing ->
                        go (i + 1) prevP prevL prevU

                    Just ( index, pivot ) ->
                        let
                            _ =
                                Debug.log "\nindex" index

                            swappedU =
                                List.Extra.swapAt index i prevU

                            _ =
                                logMatrix "swappedU" swappedU

                            swappedP =
                                prevP
                                    |> List.Extra.swapAt index i

                            finalP =
                                swappedP

                            _ =
                                logMatrix "swappedP" swappedP

                            u_i =
                                List.Extra.getAt i swappedU
                                    |> Maybe.withDefault []

                            ( _, finalL, finalU ) =
                                List.foldr
                                    (\( u_j, l_j ) ( j, accL, accU ) ->
                                        let
                                            ( nextL, nextU ) =
                                                if j <= i then
                                                    ( l_j, u_j )

                                                else
                                                    let
                                                        u_ji =
                                                            List.Extra.getAt i u_j
                                                                |> Maybe.withDefault 0

                                                        l_ji =
                                                            u_ji / pivot
                                                    in
                                                    ( List.Extra.setAt i l_ji l_j
                                                    , List.map2 (-)
                                                        u_j
                                                        (List.map ((*) l_ji) u_i)
                                                    )
                                        in
                                        ( j - 1, nextL :: accL, nextU :: accU )
                                    )
                                    ( rows - 1, [], [] )
                                    (List.map2 Tuple.pair swappedU prevL)

                            plu =
                                to2DList
                                    (mulUnsafe
                                        (mulUnsafe
                                            (from2DListUnsafe finalP)
                                            (from2DListUnsafe finalL)
                                        )
                                        (from2DListUnsafe finalU)
                                    )
                        in
                        if equivalent epsilon (from2DListUnsafe plu) a {- || True -} then
                            go (i + 1) finalP finalL finalU

                        else
                            let
                                _ =
                                    logMatrix "finalP" finalP

                                _ =
                                    logMatrix "finalU" finalU

                                _ =
                                    logMatrix "finalL" finalL

                                _ =
                                    logMatrix "wrong PLU" plu

                                _ =
                                    logMatrix "A" (to2DList a)
                            in
                            { p = from2DListUnsafe finalP
                            , l = from2DListUnsafe finalL
                            , u = from2DListUnsafe finalU
                            }

        eyeN : List (List Float)
        eyeN =
            to2DList (eye rows)
    in
    go 0 eyeN eyeN (to2DList a)


{-| Performs lu factorization
-}
luNoPivotSingle : Matrix -> Matrix
luNoPivotSingle ((Mat a) as a_) =
    let
        lu : Matrix
        lu =
            zeroes a.dimensions

        --i is inner loop j outer loop
        -- bind computelem to a
        luCompute : ( Int, Int ) -> Matrix -> Matrix
        luCompute index dest =
            luComputeElem index a_ dest

        indices : List ( Int, Int )
        indices =
            genIndices a.dimensions
    in
    List.foldl luCompute lu indices


genIndices : ( Int, Int ) -> List ( Int, Int )
genIndices ( i_range, j_range ) =
    let
        listj : List Int
        listj =
            List.concatMap (List.repeat i_range) <| List.range 1 j_range

        listi : List Int
        listi =
            List.concat <| List.repeat j_range (List.range 1 i_range)
    in
    List.map2 (\a b -> ( a, b )) listi listj


luComputeElem : ( Int, Int ) -> Matrix -> Matrix -> Matrix
luComputeElem ( i, j ) original lu =
    let
        getWithDefault : ( Int, Int ) -> Matrix -> Float
        getWithDefault index m =
            Maybe.withDefault 0.0 (get index m)

        aij : Float
        aij =
            getWithDefault ( i, j ) original

        compute : Int -> Float
        compute index =
            getWithDefault ( i, index ) lu * getWithDefault ( index, j ) lu
    in
    if i > j then
        let
            tiny : Float
            tiny =
                10 ^ -40

            bjj : Float
            bjj =
                Maybe.withDefault tiny <| get ( j, j ) lu

            k : List Int
            k =
                List.range 1 (j - 1)
        in
        -- NRIC 2.3.13
        set ( i, j ) ((aij - (List.sum <| List.map compute k)) / bjj) lu

    else
        let
            k : List Int
            k =
                List.range 1 (i - 1)
        in
        -- NRIC 2.3.12
        set ( i, j ) (aij - (List.sum <| List.map compute k)) lu


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

        check_r_bounds : Bool
        check_r_bounds =
            0 < r_index && r_index <= arows

        check_c_bounds : Bool
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

        check_r_bounds : Bool
        check_r_bounds =
            0 < r_index && r_index <= arows

        check_c_bounds : Bool
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
            , elements = Array.Extra.map2 (+) a.elements b.elements
            }
            |> Ok

    else
        let
            adims : String
            adims =
                dimToString a

            bdims : String
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
        fromArray a.dimensions <| Array.Extra.map2 f a.elements b.elements

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
    let
        epsilon =
            10 ^ -14

        n : Int
        n =
            numRows a

        go : Int -> List (List Float) -> Result String Matrix
        go i acc =
            if i >= n then
                from2DList (List.map (List.drop n) acc)

            else
                case findPivot epsilon i acc of
                    Nothing ->
                        Err "Matrix is singular"

                    Just ( j, pivot ) ->
                        let
                            swapped : List (List Float)
                            swapped =
                                List.Extra.swapAt i j acc

                            scaled : List (List Float)
                            scaled =
                                List.Extra.updateAt
                                    i
                                    (List.map (\v -> v / pivot))
                                    swapped

                            scaledRow : List Float
                            scaledRow =
                                scaled
                                    |> List.drop i
                                    |> List.head
                                    |> Maybe.withDefault []

                            reduced : List (List Float)
                            reduced =
                                scaled
                                    |> List.indexedMap
                                        (\r row ->
                                            if r == i then
                                                row

                                            else
                                                let
                                                    l : Float
                                                    l =
                                                        row
                                                            |> List.drop i
                                                            |> List.head
                                                            |> Maybe.withDefault 1
                                                in
                                                List.map2 (\v s -> v - s * l) row scaledRow
                                        )
                        in
                        go (i + 1) reduced
    in
    if numColumns a /= n then
        Err "Could not invert matrix, it's not a square"

    else
        go 0 (List.map2 (++) (to2DList a) (to2DList (eye n)))


findPivot : Float -> Int -> List (List Float) -> Maybe ( Int, Float )
findPivot epsilon i m =
    let
        go j queue best =
            case queue of
                [] ->
                    Maybe.map (\( index, pivot, _ ) -> ( index, pivot )) best

                head :: tail ->
                    let
                        next : Maybe ( Int, Float, Float )
                        next =
                            List.Extra.getAt i head
                                |> Maybe.andThen
                                    (\pivot ->
                                        let
                                            score : Float
                                            score =
                                                abs pivot

                                            candidate : ( Int, Float, Float )
                                            candidate =
                                                ( j, pivot, score )
                                        in
                                        if score < epsilon then
                                            best

                                        else
                                            case best of
                                                Nothing ->
                                                    Just candidate

                                                Just ( _, _, previousScore ) ->
                                                    if score > previousScore then
                                                        Just candidate

                                                    else
                                                        best
                                    )
                    in
                    go (j + 1) tail (next |> Maybe.Extra.orElse best)
    in
    go i (List.drop i m) Nothing


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
            bs : List Matrix
            bs =
                getColumns b

            single : Matrix
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
    if numColumns b == 1 then
        if numRows b == numColumns a then
            if numColumns a == numRows a then
                Ok (applySubstitution (luNoPivotSingle a) b)

            else
                Err "A is not square"

        else
            Err "Dimensions of A and b do not match"

    else
        Err "b is not a vector."


{-| Applies forward and backward substitution, decoupling substitution from
computing lu decomp
-}
applySubstitution : Matrix -> Matrix -> Matrix
applySubstitution single b =
    let
        brows : Int
        brows =
            numRows b

        z : Matrix
        z =
            zeroes ( brows, 1 )

        is : List Int
        is =
            List.range 1 (numRows b)

        fsBound : Int -> Matrix -> Matrix
        fsBound i y_ =
            forwardSubstitution i single b y_

        y : Matrix
        y =
            List.foldl fsBound z is

        bsBound : Int -> Matrix -> Matrix
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
        getWithDefault : ( Int, Int ) -> Matrix -> Float
        getWithDefault index m =
            Maybe.withDefault 0.0 (get index m)

        getVecDefault : Int -> Matrix -> Float
        getVecDefault index v =
            getWithDefault ( index, 1 ) v

        yi : Float
        yi =
            case i of
                1 ->
                    getVecDefault i b

                _ ->
                    let
                        bi : Float
                        bi =
                            getVecDefault i b

                        j : List Int
                        j =
                            List.range 1 (i - 1)

                        lijs : List Float
                        lijs =
                            List.map (\curr_j -> getWithDefault ( i, curr_j ) l) j

                        yjs : List Float
                        yjs =
                            List.map (\curr_j -> getVecDefault curr_j y) j

                        sum : Float
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
        getWithDefault : ( Int, Int ) -> Matrix -> Float
        getWithDefault index m =
            Maybe.withDefault 0.0 (get index m)

        getVecDefault : Int -> Matrix -> Float
        getVecDefault index v =
            getWithDefault ( index, 1 ) v

        uii : Float
        uii =
            getWithDefault ( i, i ) u

        xrows : Int
        xrows =
            numRows x

        xi : Float
        xi =
            if i == xrows then
                getVecDefault i y / uii

            else
                let
                    --aij = getWithDefault ( i, j ) original
                    yi : Float
                    yi =
                        getVecDefault i y

                    j : List Int
                    j =
                        List.range (i + 1) xrows

                    uijs : List Float
                    uijs =
                        List.map (\curr_j -> getWithDefault ( i, curr_j ) u) j

                    xjs : List Float
                    xjs =
                        List.map (\curr_j -> getVecDefault curr_j x) j

                    sum : Float
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
            single : Matrix
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
    in
    if acols == 1 && bcols == 1 && arows == brows then
        Array.Extra.map2 (*) a.elements b.elements
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
            getDefault : ( Int, Int ) -> Matrix -> Float
            getDefault x v =
                Maybe.withDefault 0.0 <| get x v

            ax : Float
            ax =
                getDefault ( 1, 1 ) a

            ay : Float
            ay =
                getDefault ( 2, 1 ) a

            az : Float
            az =
                getDefault ( 3, 1 ) a

            bx : Float
            bx =
                getDefault ( 1, 1 ) b

            by : Float
            by =
                getDefault ( 2, 1 ) b

            bz : Float
            bz =
                getDefault ( 3, 1 ) b

            i : Float
            i =
                (ay * bz) - (by * az)

            j : Float
            j =
                -1 * ((ax * bz) - (bx * az))

            k : Float
            k =
                (ax * by) - (bx * ay)
        in
        Ok (vec <| [ i, j, k ])

    else
        Err "One or both vectors are malformed."


{-| Checks if two matrices are equivalent within some epsilon.

    epsilon =
        10 ^ -4

    is_equivalent =
        equivalent epsilon a b

-}
equivalent : Float -> Matrix -> Matrix -> Bool
equivalent epsilon (Mat a) (Mat b) =
    (a.dimensions == b.dimensions)
        && (Array.Extra.map2
                (\ae be -> abs (ae - be) < epsilon)
                a.elements
                b.elements
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
        arows : Int
        arows =
            numRows a

        brows : Int
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
toString a =
    a
        |> to2DList
        |> List.map
            (\row ->
                row
                    |> List.map String.fromFloat
                    |> String.join " "
            )
        |> String.join "\n"


{-| Change matrix into string form, such as what would be displayed in the terminal.

This version aligns the columns.

    Matrix.toString (Matrix.sMul 100 (Matrix.eye 3)) ==

"""
100 0 0
0 100 0
0 0 100
"""

-}
toAlignedString : Matrix -> String
toAlignedString a =
    a
        |> to2DList
        |> List.map (\row -> List.map String.fromFloat row)
        |> List.Extra.transpose
        |> List.map (Matrix.Format.alignColumn ".")
        |> List.Extra.transpose
        |> List.map (String.join " ")
        |> String.join "\n"


toFractionString : Matrix -> String
toFractionString a =
    a
        |> to2DList
        |> List.map (\row -> List.map toFraction row)
        |> List.Extra.transpose
        |> List.map (Matrix.Format.alignColumn "/")
        |> List.Extra.transpose
        |> List.map (String.join " ")
        |> String.join "\n"


toFraction : Float -> String
toFraction f =
    let
        below =
            floor f

        above =
            ceiling f

        go budget ln ld un ud =
            let
                mn =
                    ln + un

                md =
                    ld + ud

                mid =
                    toFloat mn / toFloat md
            in
            if budget <= 0 || mid == f then
                let
                    g =
                        gcd (abs mn) (abs md)

                    gcd a b =
                        if a < b then
                            gcd b a

                        else if b == 0 then
                            a

                        else
                            gcd (modBy b a) b
                in
                if md // g == 1 then
                    String.fromInt (mn // g)

                else
                    String.fromInt (mn // g) ++ "/" ++ String.fromInt (md // g)

            else if f < mid then
                go (budget - 1) ln ld mn md

            else
                go (budget - 1) mn md un ud
    in
    go 100 below 1 above 1


{-| Helper to re-2dify a flat matrix
-}
make2D : Int -> List a -> List (List a)
make2D num_row_elem list =
    List.Extra.greedyGroupsOf num_row_elem list


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
    in
    description_string ++ toString a ++ "\n"


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
