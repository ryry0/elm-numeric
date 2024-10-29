module TestPLU exposing (fixed, fuzzy)

import Expect
import Fuzzers
import List.Extra
import Matrix exposing (Matrix)
import Test exposing (Test, describe, fuzz, test)


fuzzy : Test
fuzzy =
    fuzz Fuzzers.squareMatrix
        "PLU decomposition is correct - fuzzy"
        checkPLU


simple : Float -> Matrix
simple n =
    Matrix.initialize ( 1, 1 ) (\_ _ -> n)


fixed : Test
fixed =
    [ simple 1000
    , Matrix.from2DList
        [ [ 0, 5, 7 ]
        , [ 4, 2, 1 ]
        , [ 2, 7, 9 ]
        ]
        |> Result.withDefault (simple 0)
    , Matrix.from2DList
        [ [ 1, 1, 0 ]
        , [ 2, 1, -1 ]
        , [ 3, -1, -1 ]
        ]
        |> Result.withDefault (simple 0)
    , Matrix.from2DList
        [ [ 1, 2, 1 ]
        , [ 1, 2, 2 ]
        , [ 2, 1, 1 ]
        ]
        |> Result.withDefault (simple 0)
    , Matrix.from2DList
        [ [ 1, 2, 1, 2, 1 ]
        , [ 2, 4, 2, 4, 1 ]
        , [ 1, 2, 1, 3, 2 ]
        ]
        |> Result.withDefault (simple 0)

    -- , Matrix.from2DList
    --     [ [ 1, 2, 1 ]
    --     , [ 1, 2, 2 ]
    --     , [ 2, 4, 1 ]
    --     , [ 3, 2, 1 ]
    --     ]
    --     |> Result.withDefault (simple 0)
    -- , Matrix.from2DList
    --     [ [ -10, -1, 0, 0 ]
    --     , [ -1, 0, 0, -1 ]
    --     , [ 0, 0, -1, 0 ]
    --     , [ 0, -1, 0, 0 ]
    --     ]
    --     |> Result.withDefault (simple 0)
    ]
        |> List.map (\m -> test ("PLU Example - A:\n" ++ Matrix.toAlignedString m) <| \_ -> checkPLU m)
        |> describe "PLU decomposition is correct - examples"
        |> Test.only


checkPLU : Matrix -> Expect.Expectation
checkPLU a =
    let
        { p, l, u } =
            Matrix.luDecomp a
    in
    Expect.all
        [ \_ -> checkIsPermutation p
        , \_ -> checkIsUnitLower l
        , \_ -> checkIsUpper u
        , \_ ->
            case
                Matrix.mul p l
                    |> Result.andThen (\pl -> Matrix.mul pl u)
            of
                Err msg ->
                    Expect.fail msg

                Ok plu ->
                    let
                        lu =
                            Matrix.mul l u
                                |> Result.withDefault (Matrix.eye 0)
                    in
                    Matrix.equivalent (10 ^ -4) plu a
                        |> Expect.equal True
                        |> Expect.onFail
                            ("PLU != A.\nLU:\n"
                                ++ Matrix.toAlignedString lu
                                ++ "\nPLU:\n"
                                ++ Matrix.toAlignedString plu
                            )
        ]
        ()


checkIsPermutation : Matrix -> Expect.Expectation
checkIsPermutation mat =
    let
        rows : List (List Float)
        rows =
            Matrix.to2DList mat

        cols : List (List Float)
        cols =
            Matrix.to2DList (Matrix.transpose mat)

        check : List (List Float) -> Bool
        check vectors =
            List.all
                (\vec ->
                    List.all (\c -> c == 0 || c == 1) vec
                        && (List.Extra.count (\c -> c == 1) vec == 1)
                )
                vectors
    in
    (check rows && check cols)
        |> Expect.equal True
        |> Expect.onFail ("Not a permutation matrix:\n" ++ Matrix.toAlignedString mat)


checkIsUnitLower : Matrix -> Expect.Expectation
checkIsUnitLower mat =
    mat
        |> Matrix.to2DList
        |> List.indexedMap
            (\rowIndex row ->
                row
                    |> List.indexedMap
                        (\colIndex cell ->
                            if rowIndex < colIndex then
                                cell == 0

                            else if rowIndex == colIndex then
                                cell == 1

                            else
                                True
                        )
                    |> List.all identity
            )
        |> List.all identity
        |> Expect.equal True
        |> Expect.onFail ("Not a lower triangular:\n" ++ Matrix.toAlignedString mat)


checkIsUpper : Matrix -> Expect.Expectation
checkIsUpper mat =
    mat
        |> Matrix.to2DList
        |> List.indexedMap
            (\rowIndex row ->
                row
                    |> List.indexedMap
                        (\colIndex cell ->
                            rowIndex <= colIndex || cell == 0
                        )
                    |> List.all identity
            )
        |> List.all identity
        |> Expect.equal True
        |> Expect.onFail ("Not a lower triangular:\n" ++ Matrix.toAlignedString mat)