module Tests exposing (..)

import Expect
import Matrix exposing (Matrix)
import Test exposing (Test, test)


suite : Test
suite =
    test "Invert simple matrix" <|
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
            flip
                |> Matrix.invert
                |> Expect.equal (Ok flip)
