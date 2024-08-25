module Tests exposing (fuzzed, simple)

import Expect
import Fuzz exposing (Fuzzer)
import Matrix exposing (Matrix)
import Test exposing (Test, describe, fuzz, test)


simple : Test
simple =
    describe "Invert simple matrices"
        [ test "Invert simple matrix" <|
            \_ ->
                let
                    flip : Matrix
                    flip =
                        Matrix.initialize ( 2, 2 )
                            (\i ->
                                if i == 0 || i == 3 then
                                    0

                                else
                                    1
                            )
                in
                canInvert flip
        , test "Invert simple matrix #2" <|
            \_ ->
                let
                    hundred : Matrix
                    hundred =
                        Matrix.from2DList
                            [ [ 0, 0, 0, -100 ]
                            , [ 0, 0, -100, 0 ]
                            , [ -100, 0, 0, 0 ]
                            , [ -100, -100, 0, 0 ]
                            ]
                            |> force
                in
                canInvert hundred
        ]


force : Result error a -> a
force res =
    case res of
        Ok v ->
            v

        Err _ ->
            Debug.todo "force"


fuzzed : Test
fuzzed =
    fuzz squareMatrixFuzzer
        "Invert fuzzed matrix"
        canInvert


canInvert : Matrix -> Expect.Expectation
canInvert m =
    let
        ( n, _ ) =
            Matrix.size m
    in
    case Matrix.invert m of
        Ok inv ->
            case Matrix.mul m inv of
                Err e ->
                    Expect.fail e

                Ok maybeEye ->
                    if Matrix.equivalent (10 ^ -4) maybeEye (Matrix.eye n) then
                        Expect.pass

                    else
                        Expect.fail "Inverse is wrong"

        Err "Matrix is singular" ->
            Expect.pass

        Err e ->
            Expect.fail e


squareMatrixFuzzer : Fuzzer Matrix
squareMatrixFuzzer =
    let
        bound : Float
        bound =
            10 ^ 4
    in
    Fuzz.intRange 1 5
        |> Fuzz.andThen
            (\n ->
                Fuzz.listOfLength (n * n) (Fuzz.floatRange -bound bound)
                    |> Fuzz.filterMap
                        (\items -> Matrix.fromList ( n, n ) items |> Result.toMaybe)
            )
