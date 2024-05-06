module Elm3d.Node exposing
    ( Node
    , cube, obj, light, group
    , withPosition, withRotation, withScale
    , withRotationX, withRotationY, withRotationZ
    , Context, withOnUpdate
    , rotateX, rotateY, rotateZ
    , isDirectionalLight
    , toPosition, toRotation, toScale
    , toRotationX, toRotationY, toRotationZ
    , toEntity, toUpdateFunction, toObjFileUrls
    )

{-|

@docs Node
@docs cube, obj, light, group

@docs withPosition, withRotation, withScale
@docs withRotationX, withRotationY, withRotationZ

@docs Context, withOnUpdate

@docs rotateX, rotateY, rotateZ

@docs isDirectionalLight

@docs toPosition, toRotation, toScale
@docs toRotationX, toRotationY, toRotationZ
@docs toEntity, toUpdateFunction, toObjFileUrls

-}

import Elm3d.Asset
import Elm3d.Camera exposing (Camera)
import Elm3d.Color exposing (Color)
import Elm3d.Entities.Cube.TextureColor
import Elm3d.Entities.Obj
import Elm3d.Input
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Texture exposing (Texture)
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector3 exposing (Vector3)
import Elm3d.Vector4 exposing (Vector4)
import Math.Matrix4
import WebGL
import WebGL.Settings.DepthTest


type Node
    = Node Internals


type Mesh
    = Cube { size : Float, texture : Texture }
    | Obj { url : String }
    | Group (List Node)
    | DirectionalLight


type alias Internals =
    { mesh : Mesh
    , transform : Transform3d
    , onInput : Maybe (Elm3d.Input.Event -> Node -> Node)
    , onUpdate : Maybe (Context -> Node -> Node)
    }


cube :
    { size : Float
    , texture : Texture
    }
    -> Node
cube props =
    Node
        { mesh = Cube props
        , onInput = Nothing
        , onUpdate = Nothing
        , transform = Elm3d.Transform3d.none
        }


obj : { url : String } -> Node
obj props =
    Node
        { mesh = Obj props
        , onInput = Nothing
        , onUpdate = Nothing
        , transform = Elm3d.Transform3d.none
        }


group : List Node -> Node
group children =
    Node
        { mesh = Group children
        , onInput = Nothing
        , onUpdate = Nothing
        , transform = Elm3d.Transform3d.none
        }


light : { direction : Vector3 } -> Node
light props =
    Node
        { mesh = DirectionalLight
        , onInput = Nothing
        , onUpdate = Nothing
        , transform =
            Elm3d.Transform3d.none
                |> Elm3d.Transform3d.withRotation props.direction
        }


withPosition : Vector3 -> Node -> Node
withPosition props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPosition props node.transform }


withRotation : Vector3 -> Node -> Node
withRotation props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotation props node.transform }


withScale : Vector3 -> Node -> Node
withScale props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withScale props node.transform }


rotateX : Float -> Node -> Node
rotateX delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withRotationX
                    (delta + Elm3d.Transform3d.toRotationX node.transform)
                    node.transform
        }


rotateY : Float -> Node -> Node
rotateY delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withRotationY
                    (delta + Elm3d.Transform3d.toRotationY node.transform)
                    node.transform
        }


rotateZ : Float -> Node -> Node
rotateZ delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withRotationZ
                    (delta + Elm3d.Transform3d.toRotationZ node.transform)
                    node.transform
        }


withRotationX : Float -> Node -> Node
withRotationX props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationX props node.transform }


withRotationY : Float -> Node -> Node
withRotationY props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationY props node.transform }


withRotationZ : Float -> Node -> Node
withRotationZ props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationZ props node.transform }


type alias Context =
    { dt : Float
    , time : Float
    }


withOnUpdate : (Context -> Node -> Node) -> Node -> Node
withOnUpdate props (Node node) =
    Node { node | onUpdate = Just props }



-- READING


isDirectionalLight : Node -> Bool
isDirectionalLight (Node node) =
    case node.mesh of
        DirectionalLight ->
            True

        _ ->
            False


toPosition : Node -> Vector3
toPosition (Node node) =
    Elm3d.Transform3d.toPosition node.transform


toRotation : Node -> Vector3
toRotation (Node node) =
    Elm3d.Transform3d.toRotation node.transform


toScale : Node -> Vector3
toScale (Node node) =
    Elm3d.Transform3d.toScale node.transform


toRotationX : Node -> Float
toRotationX (Node node) =
    Elm3d.Transform3d.toRotationX node.transform


toRotationY : Node -> Float
toRotationY (Node node) =
    Elm3d.Transform3d.toRotationY node.transform


toRotationZ : Node -> Float
toRotationZ (Node node) =
    Elm3d.Transform3d.toRotationZ node.transform


toEntity :
    Matrix4
    ->
        { camera : Matrix4
        , light : Maybe Vector3
        , assets : Elm3d.Asset.Model
        }
    -> Node
    -> List WebGL.Entity
toEntity groupMatrix props ((Node { transform, mesh }) as node) =
    case mesh of
        DirectionalLight ->
            []

        Group children ->
            List.concatMap
                (toEntity
                    (Elm3d.Transform3d.toMatrix4 transform)
                    props
                )
                children

        Obj { url } ->
            case Elm3d.Asset.findObj url props.assets of
                Just data ->
                    [ Elm3d.Entities.Obj.toEntity data
                        { modelView =
                            Math.Matrix4.mul groupMatrix
                                (Elm3d.Transform3d.toMatrix4 transform)
                        , camera = props.camera
                        , lightDirection =
                            props.light
                                |> Maybe.withDefault Elm3d.Vector3.zero
                        }
                    ]

                Nothing ->
                    []

        Cube { size, texture } ->
            case texture of
                Elm3d.Texture.Color color_ ->
                    [ Elm3d.Entities.Cube.TextureColor.toEntity
                        { scale = size
                        , color = color_
                        , modelView =
                            Math.Matrix4.mul groupMatrix
                                (Elm3d.Transform3d.toMatrix4 transform)
                        , camera = props.camera
                        , lightDirection =
                            props.light
                                |> Maybe.withDefault Elm3d.Vector3.zero
                        }
                    ]


toObjFileUrls : Node -> List String
toObjFileUrls (Node node) =
    case node.mesh of
        Group children ->
            List.concatMap toObjFileUrls children

        Obj { url } ->
            [ url ]

        _ ->
            []


toUpdateFunction : Node -> Maybe (Context -> Node -> Node)
toUpdateFunction (Node node) =
    case node.mesh of
        Group children ->
            if hasUpdateFunction (Node node) || List.any hasUpdateFunction children then
                Just
                    (\context _ ->
                        case node.onUpdate of
                            Nothing ->
                                Node { node | mesh = Group (List.map (update context) children) }

                            Just onUpdate ->
                                let
                                    (Node updatedGroupNode) =
                                        onUpdate context (Node node)
                                in
                                Node { updatedGroupNode | mesh = Group (List.map (update context) children) }
                    )

            else
                Nothing

        _ ->
            node.onUpdate


hasUpdateFunction : Node -> Bool
hasUpdateFunction (Node node) =
    node.onUpdate /= Nothing


update : Context -> Node -> Node
update context (Node node) =
    case node.onUpdate of
        Just fn ->
            fn context (Node node)

        Nothing ->
            Node node
