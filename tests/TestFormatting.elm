module TestFormatting exposing (suite)

import Expect
import Matrix.Format
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test alignColumn"
        [ test "Aligns zeroes" <|
            \_ ->
                Matrix.Format.alignColumn "."
                    [ "-4.69"
                    , "0"
                    , "0"
                    ]
                    |> Expect.equalLists
                        [ "-4.69"
                        , " 0.  "
                        , " 0.  "
                        ]
        , test "aligns when longer on the right" <|
            \_ ->
                Matrix.Format.alignColumn "."
                    [ "0.000469"
                    , "4.69"
                    , "4.69"
                    ]
                    |> Expect.equalLists
                        [ "0.000469"
                        , "4.69    "
                        , "4.69    "
                        ]
        , test "aligns when longer on the left" <|
            \_ ->
                Matrix.Format.alignColumn "."
                    [ "0.000469"
                    , "25.97"
                    , "25.97"
                    ]
                    |> Expect.equalLists
                        [ " 0.000469"
                        , "25.97    "
                        , "25.97    "
                        ]
        ]
