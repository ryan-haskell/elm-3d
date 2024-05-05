module Elm3d.Node exposing
    ( Node
    , cube
    , withPosition, withRotationX
    , withTextureColor
    , Context, withOnUpdate
    , toRotationX
    , toEntity, toUpdateFunction
    , toRotationY, withRotationY
    )

{-|

@docs Node
@docs cube

@docs withPosition, withRotationX
@docs withTextureColor
@docs Context, withOnUpdate
@docs toRotationX

@docs withTextureColor


### Could be internal

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


withPosition : Vector3 -> Node -> Node
withPosition props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPosition props node.transform }


withRotationX : Float -> Node -> Node
withRotationX props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationX props node.transform }


withRotationY : Float -> Node -> Node
withRotationY props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationY props node.transform }


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


toRotationX : Node -> Float
toRotationX (Node node) =
    Elm3d.Transform3d.toRotationX node.transform


toRotationY : Node -> Float
toRotationY (Node node) =
    Elm3d.Transform3d.toRotationY node.transform


toEntity : { camera : Matrix4 } -> Node -> WebGL.Entity
toEntity { camera } ((Node { texture, transform, mesh }) as node) =
    case ( mesh, texture ) of
        ( Cube { size }, TextureColor color ) ->
            Elm3d.Entities.Cube.TextureColor.toEntity
                { scale = size
                , color = color
                , modelView = Elm3d.Transform3d.toMatrix4 transform
                , camera = camera
                }


toUpdateFunction : Node -> Maybe (Context -> Node -> Node)
toUpdateFunction (Node node) =
    node.onUpdate
