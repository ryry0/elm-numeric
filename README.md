# Elm Numeric

A matrix library for Elm, written in pure elm!
This library aims to be a reasonably complete suite of linear algebra tools.

Some highlights of this library include generic sized matrices, inverses,
transposes, LU decomposition and more!

It's very easy to get started.

```elm
     import Matrix as Mt

     identity = Mt.eye 3 -- make a 3x3 identity matrix
     identity |> Mt.debugPrint -- view the matrix in the repl
     identity |> Mt.toString -- view the matrix as a formatted string with newlines

     -- return the matrix as a list of lists:
     -- [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
     identity |> Mt.to2DList

     -- return the matrix as a flat list
     -- [1, 0, 0, 0, 1, 0, 0, 0, 1]
     identity |> Mt.toFlatList

     -- multiply a matrix [[1, 1, 1], [2, 2, 2], [3, 3, 3]] and a column vector [4, 5, 6]
     a = Mt.mat [[1, 1, 1], [2, 2, 2], [3, 3, 3]]
     b = vec [4, 5, 6]
     result = Mt.mul a b
```

Check the documentation for the full list of features.

TODO: I would appreciate any help!
* Interop with webgl
* Interop with plotting, such as elm-visualization
* Create LaTeX strings
* Make it faster!
