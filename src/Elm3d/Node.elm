module Elm3d.Node exposing
    ( Node
    , cube
    , light
    , withPosition
    , withRotationX, withRotationY, withRotationZ
    , withTextureColor
    , Context, withOnUpdate
    , isDirectionalLight
    , toPosition, toRotation, toScale
    , toRotationX, toRotationY, toRotationZ
    , toEntity, toUpdateFunction
    )

{-|

@docs Node
@docs cube
@docs light

@docs withPosition, withRotation, withScale
@docs withRotationX, withRotationY, withRotationZ
@docs withTextureColor
@docs Context, withOnUpdate

@docs isDirectionalLight

@docs toPosition, toRotation, toScale
@docs toRotationX, toRotationY, toRotationZ
@docs toEntity, toUpdateFunction

-}

import Elm3d.Camera exposing (Camera)
import Elm3d.Color exposing (Color)
import Elm3d.Entities.Cube.TextureColor
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector3 exposing (Vector3)
import Elm3d.Vector4 exposing (Vector4)
import WebGL
import WebGL.Settings.DepthTest


type Node
    = Node Internals


type Mesh
    = Cube { size : Float }
    | DirectionalLight


type alias Internals =
    { mesh : Mesh
    , transform : Transform3d
    , texture : Texture
    , onUpdate : Maybe (Context -> Node -> Node)
    }


type Texture
    = TextureColor Vector4


cube : { size : Float } -> Node
cube props =
    Node
        { mesh = Cube props
        , texture = TextureColor Elm3d.Vector4.one
        , onUpdate = Nothing
        , transform = Elm3d.Transform3d.none
        }


light : { direction : Vector3 } -> Node
light props =
    Node
        { mesh = DirectionalLight
        , texture = TextureColor Elm3d.Vector4.one
        , onUpdate = Nothing
        , transform =
            Elm3d.Transform3d.none
                |> Elm3d.Transform3d.withRotation props.direction
        }


withPosition : Vector3 -> Node -> Node
withPosition props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPosition props node.transform }


withRotationX : Float -> Node -> Node
withRotationX props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationX props node.transform }


withRotationY : Float -> Node -> Node
withRotationY props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationY props node.transform }


withRotationZ : Float -> Node -> Node
withRotationZ props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationZ props node.transform }


withTextureColor : Color -> Node -> Node
withTextureColor props (Node node) =
    Node { node | texture = TextureColor props }


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
    { camera : Matrix4
    , light : Maybe Vector3
    }
    -> Node
    -> Maybe WebGL.Entity
toEntity props ((Node { texture, transform, mesh }) as node) =
    case ( mesh, texture ) of
        ( DirectionalLight, _ ) ->
            Nothing

        ( Cube { size }, TextureColor color ) ->
            Just <|
                Elm3d.Entities.Cube.TextureColor.toEntity
                    { scale = size
                    , color = color
                    , modelView = Elm3d.Transform3d.toMatrix4 transform
                    , camera = props.camera
                    , lightDirection =
                        props.light
                            |> Maybe.withDefault Elm3d.Vector3.zero
                    }


toUpdateFunction : Node -> Maybe (Context -> Node -> Node)
toUpdateFunction (Node node) =
    node.onUpdate
