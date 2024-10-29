module TestInverse exposing (fuzzed, simple)

import Expect
import Fuzzers
import Matrix exposing (Matrix)
import Matrix.Format
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
                            (\r c ->
                                if r == c then
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
        , test "Invert simple matrix #3" <|
            \_ ->
                let
                    hundred : Matrix
                    hundred =
                        Matrix.fromList ( 3, 3 )
                            [ -10000, -4.69, -25.97, -10000, 0, 0, -10000, 0, 0 ]
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
    fuzz Fuzzers.squareMatrix
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
                        let
                            toIndentedString : Matrix -> String
                            toIndentedString matrix =
                                matrix
                                    |> Matrix.toAlignedString
                                    |> Matrix.Format.indent "  "

                            message : List String
                            message =
                                [ "Inverse is wrong for matrix:"
                                , toIndentedString m
                                , "the calculated inverse was:"
                                , toIndentedString inv
                                , "but multiplying it by the original matrix gave:"
                                , toIndentedString maybeEye
                                , "instead of the identity matrix"
                                ]
                        in
                        Expect.fail (String.join "\n" message)

        Err "Matrix is singular" ->
            Expect.pass

        Err e ->
            Expect.fail e
