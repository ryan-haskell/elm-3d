module Elm3d.File.Mtl exposing
    ( Data
    , parse
    )

import Dict exposing (Dict)
import Elm3d.Url
import Elm3d.Vector3 exposing (Vector3)


type alias Data =
    { url : String
    , materials : Dict String Material
    }


type alias Material =
    { ns : Maybe Float -- 250.000000
    , ka : Maybe Vector3 -- 1.000000 1.000000 1.000000
    , kd : Maybe Vector3 -- 1.000000 1.000000 1.000000
    , ks : Maybe Vector3 -- 0.500000 0.500000 0.500000
    , ke : Maybe Vector3 -- 0.000000 0.000000 0.000000
    , ni : Maybe Float -- 1.450000
    , d : Maybe Float -- 1.000000
    , illum : Maybe Int -- 2
    , map_Kd : Maybe String -- hexagons_medieval.png
    }


newMaterial : Material
newMaterial =
    Material
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing


parse : String -> String -> Data
parse url raw =
    { url = url
    , materials =
        List.foldl updateMaterials
            { current = "???"
            , materials = Dict.empty
            }
            (String.lines raw)
            |> .materials
            |> Dict.map
                (\_ material ->
                    { material
                        | map_Kd =
                            Maybe.map
                                (Elm3d.Url.fromRelativePath url)
                                material.map_Kd
                    }
                )
    }


type alias Info =
    { current : String
    , materials : Dict String Material
    }


updateMaterials : String -> Info -> Info
updateMaterials line info =
    if String.startsWith "newmtl " line then
        { info | current = String.dropLeft 7 line }

    else if String.startsWith "Ns " line then
        updateOrCreateMaterial info <|
            \mat -> { mat | ns = parseFloat (String.dropLeft 3 line) }

    else if String.startsWith "Ni " line then
        updateOrCreateMaterial info <|
            \mat -> { mat | ni = parseFloat (String.dropLeft 3 line) }

    else if String.startsWith "d " line then
        updateOrCreateMaterial info <|
            \mat -> { mat | d = parseFloat (String.dropLeft 2 line) }

    else if String.startsWith "illum " line then
        updateOrCreateMaterial info <|
            \mat -> { mat | illum = parseInt (String.dropLeft 6 line) }

    else if String.startsWith "map_Kd " line then
        updateOrCreateMaterial info <|
            \mat -> { mat | map_Kd = Just (String.dropLeft 7 line) }

    else if String.startsWith "Ka " line then
        updateOrCreateMaterial info <|
            \mat -> { mat | ka = parseVector3 (String.dropLeft 3 line) }

    else if String.startsWith "Kd " line then
        updateOrCreateMaterial info <|
            \mat -> { mat | kd = parseVector3 (String.dropLeft 3 line) }

    else if String.startsWith "Ks " line then
        updateOrCreateMaterial info <|
            \mat -> { mat | ks = parseVector3 (String.dropLeft 3 line) }

    else if String.startsWith "Ke " line then
        updateOrCreateMaterial info <|
            \mat -> { mat | ke = parseVector3 (String.dropLeft 3 line) }

    else
        info


updateOrCreateMaterial : Info -> (Material -> Material) -> Info
updateOrCreateMaterial info updateMaterial =
    { info
        | materials =
            Dict.update info.current
                (\maybeMat ->
                    maybeMat
                        |> Maybe.withDefault newMaterial
                        |> updateMaterial
                        |> Just
                )
                info.materials
    }


parseFloat : String -> Maybe Float
parseFloat line =
    case List.filterMap String.toFloat (String.words line) of
        x :: [] ->
            Just x

        _ ->
            Nothing


parseInt : String -> Maybe Int
parseInt line =
    case List.filterMap String.toInt (String.words line) of
        x :: [] ->
            Just x

        _ ->
            Nothing


parseVector3 : String -> Maybe Vector3
parseVector3 line =
    case List.filterMap String.toFloat (String.words line) of
        x :: y :: z :: [] ->
            Just (Elm3d.Vector3.new x y z)

        _ ->
            Nothing
