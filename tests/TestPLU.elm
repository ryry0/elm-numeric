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



-- |> Test.skip


test2DList : List (List Float) -> Test
test2DList list =
    let
        m =
            list
                |> Matrix.from2DList
                |> Result.withDefault (Matrix.eye 0)
    in
    test ("PLU Example - A:\n" ++ Matrix.toAlignedString m) <|
        \_ -> checkPLU m


fixed : Test
fixed =
    describe "PLU decomposition is correct - examples"
        [ test2DList [ [ 1000 ] ]
        , test2DList
            [ [ 1, 3, 5 ]
            , [ 2, 4, 7 ]
            , [ 1, 1, 0 ]
            ]
        , test2DList
            [ [ 11, 9, 24, 2 ]
            , [ 1, 5, 2, 6 ]
            , [ 3, 17, 18, 1 ]
            , [ 2, 5, 7, 1 ]
            ]
        , test2DList
            [ [ 0, 5, 7 ]
            , [ 4, 2, 1 ]
            , [ 2, 7, 9 ]
            ]
        , test2DList
            [ [ 1, 1, 0 ]
            , [ 2, 1, -1 ]
            , [ 3, -1, -1 ]
            ]
        , test2DList
            [ [ 1, 2, 1 ]
            , [ 1, 2, 2 ]
            , [ 2, 1, 1 ]
            ]
        , test2DList
            [ [ 1, 2, 1, 2, 1 ]
            , [ 2, 4, 2, 4, 1 ]
            , [ 1, 2, 1, 3, 2 ]
            ]
        , test2DList
            [ [ 1, 2, 1 ]
            , [ 1, 2, 2 ]
            , [ 2, 4, 1 ]
            , [ 3, 2, 1 ]
            ]

        -- |> Test.only
        , test2DList
            [ [ -10, -1, 0, 0 ]
            , [ -1, 0, 0, -1 ]
            , [ 0, 0, -1, 0 ]
            , [ 0, -1, 0, 0 ]
            ]
        ]


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
                ( Matrix.mul l u, Matrix.mul p a )
            of
                ( Err msg, _ ) ->
                    Expect.fail msg

                ( _, Err msg ) ->
                    Expect.fail msg

                ( Ok lu, Ok pa ) ->
                    Matrix.equivalent (10 ^ -4) lu pa
                        |> Expect.equal True
                        |> Expect.onFail
                            ("LU != PA.\nLU:\n"
                                ++ Matrix.toAlignedString lu
                                ++ "\nPA:\n"
                                ++ Matrix.toAlignedString pa
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
        |> Expect.onFail ("Not an upper triangular:\n" ++ Matrix.toAlignedString mat)
