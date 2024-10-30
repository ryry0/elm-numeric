module TestDeterminant exposing (examples)

import Expect
import Matrix
import Test exposing (describe, test)


examples : Test.Test
examples =
    [ ( [ [ 0 ] ], 0 )
    , ( [ [ 1 ] ], 1 )
    ]
        |> List.map
            (\( mat, det ) ->
                test (Debug.toString mat) <|
                    \_ ->
                        case Matrix.from2DList mat of
                            Err msg ->
                                Expect.fail msg

                            Ok m ->
                                Matrix.determinant m
                                    |> Expect.equal (Just det)
            )
        |> describe "Determinant examples"
