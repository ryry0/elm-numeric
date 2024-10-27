module TestFormatting exposing (suite)

import Expect
import Matrix.Format
import Test exposing (Test, test)


suite : Test
suite =
    test "Test alignColumn" <|
        \_ ->
            Matrix.Format.alignColumn
                [ "-4.69"
                , "0"
                , "0"
                ]
                |> Expect.equalLists
                    [ "-4.69"
                    , " 0.  "
                    , " 0.  "
                    ]
