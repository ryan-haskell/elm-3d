module Elm3d.File.Obj exposing
    ( Attributes
    , Data
    , Error(..)
    , parse
    , toMesh
    )

import Array exposing (Array)
import Elm3d.Url
import Elm3d.Vector3 exposing (Vector3)
import Math.Vector2
import WebGL


type alias Data =
    { url : String
    , info : Info
    }


type alias Info =
    { vertices : Array GeometricVertex
    , normals : Array NormalVertex
    , textures : Array TextureVertex
    , faces : List Face
    , mtl : List String
    , faceInfo : FaceInfo
    }


type alias GeometricVertex =
    Elm3d.Vector3.Vector3


type alias NormalVertex =
    Elm3d.Vector3.Vector3


type alias TextureVertex =
    Math.Vector2.Vec2


type Error
    = Error


parse : String -> String -> Data
parse url raw =
    { url = url
    , info = parseInfo url (String.lines raw)
    }


parseInfo : String -> List String -> Info
parseInfo url lines =
    let
        info =
            parseInfoHelp lines (Info Array.empty Array.empty Array.empty [] [] { mtllib = Nothing, usemtl = Nothing })
    in
    { info
        | mtl = List.map (Elm3d.Url.fromRelativePath url) info.mtl
        , faces = List.reverse info.faces
    }


parseInfoHelp : List String -> Info -> Info
parseInfoHelp lines info =
    case lines of
        [] ->
            info

        line :: rest ->
            if String.startsWith "mtllib " line then
                let
                    { faceInfo } =
                        info

                    name =
                        String.dropLeft 7 line
                in
                parseInfoHelp rest
                    { info
                        | mtl = name :: info.mtl
                        , faceInfo = { faceInfo | mtllib = Just name }
                    }

            else if String.startsWith "usemtl " line then
                let
                    { faceInfo } =
                        info

                    name =
                        String.dropLeft 7 line
                in
                parseInfoHelp rest
                    { info
                        | faceInfo = { faceInfo | usemtl = Just name }
                    }

            else if String.startsWith "v " line then
                parseInfoHelp rest
                    { info
                        | vertices =
                            case
                                line
                                    |> String.dropLeft 2
                                    |> String.words
                                    |> List.filterMap String.toFloat
                            of
                                x :: y :: z :: _ ->
                                    info.vertices
                                        |> Array.push (Elm3d.Vector3.new x y z)

                                _ ->
                                    info.vertices
                    }

            else if String.startsWith "vn " line then
                parseInfoHelp rest
                    { info
                        | normals =
                            case
                                line
                                    |> String.dropLeft 3
                                    |> String.words
                                    |> List.filterMap String.toFloat
                            of
                                x :: y :: z :: _ ->
                                    info.normals
                                        |> Array.push (Elm3d.Vector3.new x y z)

                                _ ->
                                    info.normals
                    }

            else if String.startsWith "vt " line then
                parseInfoHelp rest
                    { info
                        | textures =
                            case
                                line
                                    |> String.dropLeft 3
                                    |> String.words
                                    |> List.filterMap String.toFloat
                            of
                                u :: v :: _ ->
                                    info.textures
                                        |> Array.push (Math.Vector2.vec2 u v)

                                _ ->
                                    info.textures
                    }

            else if String.startsWith "f " line then
                parseInfoHelp rest
                    { info
                        | faces =
                            case
                                line
                                    |> String.dropLeft 2
                                    |> toFace info.faceInfo
                            of
                                Just face ->
                                    face :: info.faces

                                Nothing ->
                                    info.faces
                    }

            else
                parseInfoHelp rest info


type alias FaceInfo =
    { mtllib : Maybe String
    , usemtl : Maybe String
    }


type Face
    = Face_V3 FaceInfo ( Int, Int, Int ) -- "1 2 3"
    | Face_VT3 FaceInfo ( Int, Int, Int ) ( Int, Int, Int ) -- "1/1 2/2 3/3"
    | Face_VN3 FaceInfo ( Int, Int, Int ) ( Int, Int, Int ) -- "1//1 2//2 3//3"
    | Face_VTN3 FaceInfo ( Int, Int, Int ) ( Int, Int, Int ) ( Int, Int, Int ) -- "1/1/1 2/2/2 3/3/3"
    | Face_V4 FaceInfo ( Int, Int, Int ) Int -- "1 2 3 4"
    | Face_VT4 FaceInfo ( Int, Int, Int ) Int ( Int, Int, Int ) Int -- "1/1 2/2 3/3"
    | Face_VN4 FaceInfo ( Int, Int, Int ) Int ( Int, Int, Int ) Int -- "1//1 2//2 3//3 4//4"
    | Face_VTN4 FaceInfo ( Int, Int, Int ) Int ( Int, Int, Int ) Int ( Int, Int, Int ) Int -- "1/1/1 2/2/2 3/3/3 4/4/4"


toTrianglePositionIndexes : Face -> List ( Int, Int, Int )
toTrianglePositionIndexes face =
    List.map decrement <|
        case face of
            Face_V3 _ ( i, j, k ) ->
                [ ( i, j, k ) ]

            Face_V4 _ ( i, j, k ) l ->
                [ ( i, j, k )
                , ( i, k, l )
                ]

            Face_VT3 _ ( i, j, k ) _ ->
                [ ( i, j, k ) ]

            Face_VT4 _ ( i, j, k ) l _ _ ->
                [ ( i, j, k )
                , ( i, k, l )
                ]

            Face_VN3 _ ( i, j, k ) _ ->
                [ ( i, j, k ) ]

            Face_VN4 _ ( i, j, k ) l _ _ ->
                [ ( i, j, k )
                , ( i, k, l )
                ]

            Face_VTN3 _ ( i, j, k ) _ _ ->
                [ ( i, j, k ) ]

            Face_VTN4 _ ( i, j, k ) l _ _ _ _ ->
                [ ( i, j, k )
                , ( i, k, l )
                ]


toFace : FaceInfo -> String -> Maybe Face
toFace faceInfo line =
    let
        words : List String
        words =
            String.words line
    in
    if String.contains "//" line then
        case List.concatMap (String.split "//" >> List.filterMap String.toInt) words of
            v1 :: n1 :: v2 :: n2 :: v3 :: n3 :: [] ->
                Just
                    (Face_VN3 faceInfo
                        ( v1, v2, v3 )
                        ( n1, n2, n3 )
                    )

            v1 :: n1 :: v2 :: n2 :: v3 :: n3 :: v4 :: n4 :: [] ->
                Just
                    (Face_VN4 faceInfo
                        ( v1, v2, v3 )
                        v4
                        ( n1, n2, n3 )
                        n4
                    )

            _ ->
                Nothing

    else
        case List.concatMap (String.split "/" >> List.filterMap String.toInt) words of
            v1 :: v2 :: v3 :: [] ->
                Just (Face_V3 faceInfo ( v1, v2, v3 ))

            v1 :: v2 :: v3 :: v4 :: [] ->
                Just (Face_V4 faceInfo ( v1, v2, v3 ) v4)

            v1 :: t1 :: v2 :: t2 :: v3 :: t3 :: [] ->
                Just
                    (Face_VT3 faceInfo
                        ( v1, v2, v3 )
                        ( t1, t2, t3 )
                    )

            v1 :: t1 :: v2 :: t2 :: v3 :: t3 :: v4 :: t4 :: [] ->
                Just
                    (Face_VT4 faceInfo
                        ( v1, v2, v3 )
                        v4
                        ( t1, t2, t3 )
                        t4
                    )

            v1 :: t1 :: n1 :: v2 :: t2 :: n2 :: v3 :: t3 :: n3 :: [] ->
                Just
                    (Face_VTN3 faceInfo
                        ( v1, v2, v3 )
                        ( t1, t2, t3 )
                        ( n1, n2, n3 )
                    )

            v1 :: t1 :: n1 :: v2 :: t2 :: n2 :: v3 :: t3 :: n3 :: v4 :: t4 :: n4 :: [] ->
                Just
                    (Face_VTN4 faceInfo
                        ( v1, v2, v3 )
                        v4
                        ( t1, t2, t3 )
                        t4
                        ( n1, n2, n3 )
                        n4
                    )

            _ ->
                Nothing


decrement : ( Int, Int, Int ) -> ( Int, Int, Int )
decrement ( a, b, c ) =
    ( a - 1, b - 1, c - 1 )


type alias Attributes =
    { position : Elm3d.Vector3.Vector3
    , normal : Elm3d.Vector3.Vector3
    , uv : Math.Vector2.Vec2
    , kd : Elm3d.Vector3.Vector3
    }


type alias MeshProps =
    { toKd : String -> String -> Elm3d.Vector3.Vector3
    }


toMesh :
    MeshProps
    -> Data
    -> WebGL.Mesh Attributes
toMesh props data =
    let
        { attributes, indices } =
            render props data
    in
    WebGL.indexedTriangles
        (Array.toList attributes)
        (Array.toList indices)


type alias MeshData =
    { attributes : Array Attributes
    , indices : Array ( Int, Int, Int )
    }


render : MeshProps -> Data -> MeshData
render props data =
    data.info.faces
        |> List.foldl (addFaceData props data.info)
            { attributes = Array.empty
            , indices = Array.empty
            }


addFaceData : MeshProps -> Info -> Face -> MeshData -> MeshData
addFaceData meshProps info face mesh =
    let
        toVertexData faceInfo =
            { kd =
                Maybe.map2 meshProps.toKd
                    faceInfo.mtllib
                    faceInfo.usemtl
                    |> Maybe.withDefault Elm3d.Vector3.one
            }

        offset =
            Array.length mesh.attributes
    in
    case face of
        Face_V3 faceInfo ( v1, v2, v3 ) ->
            let
                data =
                    toVertexData faceInfo
            in
            { attributes =
                Maybe.map3 (add3ToAttributes faceInfo mesh)
                    (fromV data v1 info)
                    (fromV data v2 info)
                    (fromV data v3 info)
                    |> Maybe.withDefault mesh.attributes
            , indices =
                mesh.indices
                    |> Array.push ( offset, offset + 1, offset + 2 )
            }

        Face_V4 faceInfo ( v1, v2, v3 ) v4 ->
            let
                data =
                    toVertexData faceInfo
            in
            { attributes =
                Maybe.map4 (add4ToAttributes faceInfo mesh)
                    (fromV data v1 info)
                    (fromV data v2 info)
                    (fromV data v3 info)
                    (fromV data v4 info)
                    |> Maybe.withDefault mesh.attributes
            , indices =
                mesh.indices
                    |> Array.push ( offset, offset + 1, offset + 2 )
                    |> Array.push ( offset, offset + 2, offset + 3 )
            }

        Face_VN3 faceInfo ( v1, v2, v3 ) ( n1, n2, n3 ) ->
            let
                data =
                    toVertexData faceInfo
            in
            { attributes =
                Maybe.map3 (add3ToAttributes faceInfo mesh)
                    (fromVN data v1 n1 info)
                    (fromVN data v2 n2 info)
                    (fromVN data v3 n3 info)
                    |> Maybe.withDefault mesh.attributes
            , indices =
                mesh.indices
                    |> Array.push ( offset, offset + 1, offset + 2 )
            }

        Face_VN4 faceInfo ( v1, v2, v3 ) v4 ( n1, n2, n3 ) n4 ->
            let
                data =
                    toVertexData faceInfo
            in
            { attributes =
                Maybe.map4 (add4ToAttributes faceInfo mesh)
                    (fromVN data v1 n1 info)
                    (fromVN data v2 n2 info)
                    (fromVN data v3 n3 info)
                    (fromVN data v4 n4 info)
                    |> Maybe.withDefault mesh.attributes
            , indices =
                mesh.indices
                    |> Array.push ( offset, offset + 1, offset + 2 )
                    |> Array.push ( offset, offset + 2, offset + 3 )
            }

        Face_VT3 faceInfo ( v1, v2, v3 ) ( t1, t2, t3 ) ->
            let
                data =
                    toVertexData faceInfo
            in
            { attributes =
                Maybe.map3 (add3ToAttributes faceInfo mesh)
                    (fromVT data v1 t1 info)
                    (fromVT data v2 t2 info)
                    (fromVT data v3 t3 info)
                    |> Maybe.withDefault mesh.attributes
            , indices =
                mesh.indices
                    |> Array.push ( offset, offset + 1, offset + 2 )
            }

        Face_VT4 faceInfo ( v1, v2, v3 ) v4 ( t1, t2, t3 ) t4 ->
            let
                data =
                    toVertexData faceInfo
            in
            { attributes =
                Maybe.map4 (add4ToAttributes faceInfo mesh)
                    (fromVT data v1 t1 info)
                    (fromVT data v2 t2 info)
                    (fromVT data v3 t3 info)
                    (fromVT data v4 t4 info)
                    |> Maybe.withDefault mesh.attributes
            , indices =
                mesh.indices
                    |> Array.push ( offset, offset + 1, offset + 2 )
                    |> Array.push ( offset, offset + 2, offset + 3 )
            }

        Face_VTN3 faceInfo ( v1, v2, v3 ) ( t1, t2, t3 ) ( n1, n2, n3 ) ->
            let
                data =
                    toVertexData faceInfo
            in
            { attributes =
                Maybe.map3 (add3ToAttributes faceInfo mesh)
                    (fromVTN data v1 t1 n1 info)
                    (fromVTN data v2 t2 n2 info)
                    (fromVTN data v3 t3 n3 info)
                    |> Maybe.withDefault mesh.attributes
            , indices =
                mesh.indices
                    |> Array.push ( offset, offset + 1, offset + 2 )
            }

        Face_VTN4 faceInfo ( v1, v2, v3 ) v4 ( t1, t2, t3 ) t4 ( n1, n2, n3 ) n4 ->
            let
                data =
                    toVertexData faceInfo
            in
            { attributes =
                Maybe.map4 (add4ToAttributes faceInfo mesh)
                    (fromVTN data v1 t1 n1 info)
                    (fromVTN data v2 t2 n2 info)
                    (fromVTN data v3 t3 n3 info)
                    (fromVTN data v4 t4 n4 info)
                    |> Maybe.withDefault mesh.attributes
            , indices =
                mesh.indices
                    |> Array.push ( offset, offset + 1, offset + 2 )
                    |> Array.push ( offset, offset + 2, offset + 3 )
            }


add3ToAttributes : FaceInfo -> MeshData -> Attributes -> Attributes -> Attributes -> Array Attributes
add3ToAttributes faceInfo mesh a b c =
    mesh.attributes
        |> Array.push a
        |> Array.push b
        |> Array.push c


add4ToAttributes : FaceInfo -> MeshData -> Attributes -> Attributes -> Attributes -> Attributes -> Array Attributes
add4ToAttributes faceInfo mesh a b c d =
    mesh.attributes
        |> Array.push a
        |> Array.push b
        |> Array.push c
        |> Array.push d


type alias FaceData =
    { kd : Vector3
    }


fromV : FaceData -> Int -> Info -> Maybe Attributes
fromV { kd } index { vertices } =
    case Array.get (index - 1) vertices of
        Just vec3 ->
            Just
                { position = vec3
                , normal = vec3
                , uv = Math.Vector2.vec2 0 0
                , kd = kd
                }

        Nothing ->
            Nothing


fromVN : FaceData -> Int -> Int -> Info -> Maybe Attributes
fromVN { kd } vi ni { vertices, normals } =
    case ( Array.get (vi - 1) vertices, Array.get (ni - 1) normals ) of
        ( Just v, Just n ) ->
            Just
                { position = v
                , normal = n
                , uv = Math.Vector2.vec2 0 0
                , kd = kd
                }

        _ ->
            Nothing


fromVT : FaceData -> Int -> Int -> Info -> Maybe Attributes
fromVT { kd } vi ti { vertices, textures } =
    case ( Array.get (vi - 1) vertices, Array.get (ti - 1) textures ) of
        ( Just v, Just t ) ->
            Just
                { position = v
                , normal = v
                , uv = t
                , kd = kd
                }

        _ ->
            Nothing


fromVTN : FaceData -> Int -> Int -> Int -> Info -> Maybe Attributes
fromVTN { kd } vi ti ni { vertices, normals, textures } =
    case ( Array.get (vi - 1) vertices, Array.get (ti - 1) textures, Array.get (ni - 1) normals ) of
        ( Just v, Just t, Just n ) ->
            Just
                { position = v
                , normal = n
                , uv = t
                , kd = kd
                }

        _ ->
            Nothing



--     Face_V4 ( i, j, k ) l ->
--         [ ( i, j, k )
--         , ( i, k, l )
--         ]
--     Face_VT3 ( i, j, k ) _ ->
--         [ ( i, j, k ) ]
--     Face_VT4 ( i, j, k ) l _ _ ->
--         [ ( i, j, k )
--         , ( i, k, l )
--         ]
--     Face_VN3 ( i, j, k ) _ ->
--         [ ( i, j, k ) ]
--     Face_VN4 ( i, j, k ) l _ _ ->
--         [ ( i, j, k )
--         , ( i, k, l )
--         ]
--     Face_VTN3 ( i, j, k ) _ _ ->
--         [ ( i, j, k ) ]
--     Face_VTN4 ( i, j, k ) l _ _ _ _ ->
--         [ ( i, j, k )
--         , ( i, k, l )
--         ]
