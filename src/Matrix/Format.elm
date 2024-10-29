module Matrix.Format exposing (alignColumn, indent)


alignColumn : String -> List String -> List String
alignColumn alignOn floats =
    let
        splat : List ( List String, String )
        splat =
            List.map (\f -> ( String.split alignOn f, f )) floats

        ( beforeDotLength, afterDotLength ) =
            List.foldl alignColumnStep ( 0, 0 ) splat

        alignColumnStep : ( List String, String ) -> ( Int, Int ) -> ( Int, Int )
        alignColumnStep ( split, _ ) ( before, after ) =
            case split of
                [ int ] ->
                    ( max before (String.length int)
                    , after
                    )

                [ int, decimal ] ->
                    ( max before (String.length int)
                    , max after (String.length decimal)
                    )

                _ ->
                    ( before, after )

        alignFloat : ( List String, String ) -> String
        alignFloat ( split, f ) =
            if afterDotLength == 0 then
                String.padLeft beforeDotLength ' ' f

            else
                let
                    ( whole, decimal ) =
                        case split of
                            [ i, d ] ->
                                ( i, d )

                            _ ->
                                ( f, "" )

                    leftPadded =
                        (whole
                            |> String.padLeft beforeDotLength ' '
                        )
                            ++ (if alignOn == "." then
                                    "."

                                else if String.contains alignOn f then
                                    alignOn

                                else
                                    " "
                               )
                            ++ decimal
                in
                leftPadded
                    |> String.padRight (beforeDotLength + afterDotLength + 1) ' '
    in
    List.map alignFloat splat


indent : String -> String -> String
indent prefix str =
    prefix
        ++ (str
                |> String.split "\n"
                |> String.join ("\n" ++ prefix)
           )
