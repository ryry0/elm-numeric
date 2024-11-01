module Matrix.Build exposing
    ( mat1
    , mat2, mat1x2, mat2x1
    , mat3, mat1x3, mat3x1, mat2x3, mat3x2
    )

{-|

@docs mat1

@docs mat2, mat1x2, mat2x1

@docs mat3, mat1x3, mat3x1, mat2x3, mat3x2

-}

import Matrix exposing (Matrix)


{-| Builds a 1x1 matrix.

    import Matrix

    mat1
        42
        |> Matrix.toString
    --> "42"

-}
mat1 :
    Float
    -> Matrix
mat1 a =
    Matrix.initialize ( 1, 1 )
        (\_ _ ->
            a
        )


{-| Builds a 2x1 matrix.

    import Matrix

    mat2x1
        ( 42
        , 1
        )
        |> Matrix.toString
    --> "42\n1"

-}
mat2x1 :
    ( Float
    , Float
    )
    -> Matrix
mat2x1 ( a, b ) =
    Matrix.initialize ( 2, 1 )
        (\r _ ->
            if r == 0 then
                a

            else
                b
        )


{-| Builds a 1x2 matrix.

    import Matrix

    mat1x2
        (42, 1)
        |> Matrix.toString
    --> "42 1"

-}
mat1x2 :
    ( Float, Float )
    -> Matrix
mat1x2 ( a, b ) =
    Matrix.initialize ( 2, 1 )
        (\_ c ->
            if c == 0 then
                a

            else
                b
        )


{-| Builds a 2x2 matrix.

    import Matrix

    mat2
        ( (42, 1)
        , (2, 3)
        )
        |> Matrix.toString
    --> "42 1\n2 3"

-}
mat2 :
    ( ( Float, Float )
    , ( Float, Float )
    )
    -> Matrix
mat2 ( ( a, b ), ( c, d ) ) =
    Matrix.initialize ( 2, 2 )
        (\r_ c_ ->
            case ( r_, c_ ) of
                ( 0, 0 ) ->
                    a

                ( 0, 1 ) ->
                    b

                ( 1, 0 ) ->
                    c

                _ ->
                    d
        )


{-| Builds a 1x3 matrix.

    import Matrix

    mat1x3
        ( (42, 1, 2)
        )
        |> Matrix.toString
    --> "42 1 2"

-}
mat1x3 :
    ( Float, Float, Float )
    -> Matrix
mat1x3 ( a, b, c ) =
    Matrix.initialize ( 1, 3 )
        (\r_ c_ ->
            case ( r_, c_ ) of
                ( 0, 0 ) ->
                    a

                ( 0, 1 ) ->
                    b

                _ ->
                    c
        )


{-| Builds a 2x3 matrix.

    import Matrix

    mat2x3
        ( (42, 1, 2)
        , (3, 4, 5)
        )
        |> Matrix.toString
    --> "42 1 2\n3 4 5"

-}
mat2x3 :
    ( ( Float, Float, Float )
    , ( Float, Float, Float )
    )
    -> Matrix
mat2x3 ( ( a, b, c ), ( d, e, f ) ) =
    Matrix.initialize ( 2, 3 )
        (\r_ c_ ->
            case ( r_, c_ ) of
                ( 0, 0 ) ->
                    a

                ( 0, 1 ) ->
                    b

                ( 0, 2 ) ->
                    c

                ( 1, 0 ) ->
                    d

                ( 1, 1 ) ->
                    e

                _ ->
                    f
        )


{-| Builds a 3x2 matrix.

    import Matrix

    mat2x3
        ( (42, 1)
        , (2, 3)
        , (4, 5)
        )
        |> Matrix.toString
    --> "42 1\n2 3\n4 5"

-}
mat3x2 :
    ( ( Float, Float )
    , ( Float, Float )
    , ( Float, Float )
    )
    -> Matrix
mat3x2 ( ( a, b ), ( c, d ), ( e, f ) ) =
    Matrix.initialize ( 3, 2 )
        (\r_ c_ ->
            case ( r_, c_ ) of
                ( 0, 0 ) ->
                    a

                ( 0, 1 ) ->
                    b

                ( 1, 0 ) ->
                    c

                ( 1, 1 ) ->
                    d

                ( 2, 0 ) ->
                    e

                _ ->
                    f
        )


{-| Builds a 3x1 matrix.

    import Matrix

    mat2x3
        ( 42
        , 2
        , 4
        )
        |> Matrix.toString
    --> "42 1\n2 3\n4 5"

-}
mat3x1 :
    ( Float
    , Float
    , Float
    )
    -> Matrix
mat3x1 ( a, b, c ) =
    Matrix.initialize ( 3, 1 )
        (\r_ c_ ->
            case ( r_, c_ ) of
                ( 0, 0 ) ->
                    a

                ( 1, 0 ) ->
                    b

                _ ->
                    c
        )


{-| Builds a 3x3 matrix.

    import Matrix

    mat3
        ( (42, 1, 2)
        , (3, 4, 5)
        , (6, 7, 8)
        )
        |> Matrix.toString
    --> "42 1 2\n3 4 5\n6 7 8"

-}
mat3 :
    ( ( Float, Float, Float )
    , ( Float, Float, Float )
    , ( Float, Float, Float )
    )
    -> Matrix
mat3 ( ( a, b, c ), ( d, e, f ), ( g, h, i ) ) =
    Matrix.initialize ( 3, 3 )
        (\r_ c_ ->
            case ( r_, c_ ) of
                ( 0, 0 ) ->
                    a

                ( 0, 1 ) ->
                    b

                ( 0, 2 ) ->
                    c

                ( 1, 0 ) ->
                    d

                ( 1, 1 ) ->
                    e

                ( 1, 2 ) ->
                    f

                ( 2, 0 ) ->
                    g

                ( 2, 1 ) ->
                    h

                _ ->
                    i
        )
