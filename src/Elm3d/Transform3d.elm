module Elm3d.Transform3d exposing
    ( Transform3d, none
    , withPosition, withRotationX, withRotationY, withRotationZ, withScale
    , toRotationX, toRotationY
    , toMatrix4
    )

{-|

@docs Transform3d, none
@docs withPosition, withRotationX, withRotationY, withRotationZ, withScale
@docs toRotationX, toRotationY
@docs toMatrix4

-}

import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Vector3 exposing (Vector3)
import Math.Matrix4


type Transform3d
    = Transform3d Internals


type alias Internals =
    { position : Vector3
    , rotationX : Float
    , rotationY : Float
    , rotationZ : Float
    , scale : Vector3
    }


none : Transform3d
none =
    Transform3d
        { position = Elm3d.Vector3.zero
        , rotationX = 0
        , rotationY = 0
        , rotationZ = 0
        , scale = Elm3d.Vector3.one
        }


withPosition : Vector3 -> Transform3d -> Transform3d
withPosition props (Transform3d transform) =
    Transform3d { transform | position = props }


withRotationX : Float -> Transform3d -> Transform3d
withRotationX props (Transform3d transform) =
    Transform3d { transform | rotationX = props }


withRotationY : Float -> Transform3d -> Transform3d
withRotationY props (Transform3d transform) =
    Transform3d { transform | rotationY = props }


withRotationZ : Float -> Transform3d -> Transform3d
withRotationZ props (Transform3d transform) =
    Transform3d { transform | rotationZ = props }


withScale : Vector3 -> Transform3d -> Transform3d
withScale props (Transform3d transform) =
    Transform3d { transform | scale = props }


toMatrix4 : Transform3d -> Matrix4
toMatrix4 (Transform3d transform) =
    Math.Matrix4.identity
        |> Math.Matrix4.translate transform.position
        |> Math.Matrix4.rotate transform.rotationX Elm3d.Vector3.positiveX
        |> Math.Matrix4.rotate transform.rotationY Elm3d.Vector3.positiveY
        |> Math.Matrix4.rotate transform.rotationZ Elm3d.Vector3.positiveZ
        |> Math.Matrix4.scale transform.scale


toRotationX : Transform3d -> Float
toRotationX (Transform3d transform) =
    transform.rotationX


toRotationY : Transform3d -> Float
toRotationY (Transform3d transform) =
    transform.rotationY
