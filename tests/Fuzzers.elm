module Fuzzers exposing (squareMatrix)

import Fuzz exposing (Fuzzer)
import Matrix exposing (Matrix)


squareMatrix : Fuzzer Matrix
squareMatrix =
    let
        bound : Float
        bound =
            10 ^ 4
    in
    Fuzz.intRange 1 5
        |> Fuzz.andThen
            (\n ->
                Fuzz.floatRange -bound bound
                    |> Fuzz.map (\f -> (f * 100 |> round |> toFloat) / 100)
                    |> Fuzz.listOfLength (n * n)
                    |> Fuzz.filterMap
                        (\items ->
                            Matrix.fromList ( n, n ) items
                                |> Result.toMaybe
                        )
            )
