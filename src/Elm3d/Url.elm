module Elm3d.Url exposing (fromRelativePath)

import Url exposing (Url)


fromRelativePath : String -> String -> String
fromRelativePath reqUrl relative =
    let
        appendToUrlPath : Url -> String
        appendToUrlPath url =
            url.path
                |> String.split "/"
                |> List.reverse
                |> List.drop 1
                |> (::) relative
                |> List.reverse
                |> String.join "/"
    in
    Url.fromString reqUrl
        |> Maybe.map Just
        |> Maybe.withDefault (Url.fromString ("http://localhost:3000" ++ reqUrl))
        |> Maybe.map appendToUrlPath
        |> Maybe.withDefault relative
