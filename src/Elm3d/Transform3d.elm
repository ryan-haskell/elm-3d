module Elm3d.Transform3d exposing
    ( Transform3d, none
    , withPosition, withRotation, withScale
    , withRotationX, withRotationY, withRotationZ
    , toPosition, toRotation, toScale
    , toRotationX, toRotationY, toRotationZ
    , toMatrix4
    )

{-|

@docs Transform3d, none

@docs withPosition, withRotation, withScale
@docs withRotationX, withRotationY, withRotationZ

@docs combine

@docs toPosition, toRotation, toScale
@docs toRotationX, toRotationY, toRotationZ
@docs toMatrix4

-}

import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Vector3 exposing (Vector3)
import Math.Matrix4
import Math.Vector3


type Transform3d
    = Transform3d Internals


type alias Internals =
    { position : Vector3
    , rotationX : Float
    , rotationY : Float
    , rotationZ : Float
    , scale : Vector3
    }



-- let
--     combined : Matrix4
--     combined =
-- in
-- Transform3d
--     { position = Math.Matrix4.
--     }
-- TODO: I think this is wrong
-- Transform3d
--     { position = Math.Vector3.add parent.position child.position
--     , rotationX = parent.rotationX + child.rotationX
--     , rotationY = parent.rotationY + child.rotationY
--     , rotationZ = parent.rotationZ + child.rotationZ
--     , scale =
--         Elm3d.Vector3.new
--             (Elm3d.Vector3.x parent.scale * Elm3d.Vector3.x child.scale)
--             (Elm3d.Vector3.y parent.scale * Elm3d.Vector3.y child.scale)
--             (Elm3d.Vector3.z parent.scale * Elm3d.Vector3.z child.scale)
--     }


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


withRotation : Vector3 -> Transform3d -> Transform3d
withRotation props (Transform3d transform) =
    Transform3d
        { transform
            | rotationX = Elm3d.Vector3.x props
            , rotationY = Elm3d.Vector3.y props
            , rotationZ = Elm3d.Vector3.z props
        }


withScale : Vector3 -> Transform3d -> Transform3d
withScale props (Transform3d transform) =
    Transform3d { transform | scale = props }


withRotationX : Float -> Transform3d -> Transform3d
withRotationX props (Transform3d transform) =
    Transform3d { transform | rotationX = props }


withRotationY : Float -> Transform3d -> Transform3d
withRotationY props (Transform3d transform) =
    Transform3d { transform | rotationY = props }


withRotationZ : Float -> Transform3d -> Transform3d
withRotationZ props (Transform3d transform) =
    Transform3d { transform | rotationZ = props }


toMatrix4 : Transform3d -> Matrix4
toMatrix4 (Transform3d transform) =
    Math.Matrix4.makeTranslate transform.position
        |> Math.Matrix4.scale transform.scale
        |> Math.Matrix4.rotate transform.rotationX Elm3d.Vector3.positiveX
        |> Math.Matrix4.rotate transform.rotationY Elm3d.Vector3.positiveY
        |> Math.Matrix4.rotate transform.rotationZ Elm3d.Vector3.positiveZ


toPosition : Transform3d -> Vector3
toPosition (Transform3d transform) =
    transform.position


toRotation : Transform3d -> Vector3
toRotation (Transform3d transform) =
    Elm3d.Vector3.new
        transform.rotationX
        transform.rotationY
        transform.rotationZ


toScale : Transform3d -> Vector3
toScale (Transform3d transform) =
    transform.scale


toRotationX : Transform3d -> Float
toRotationX (Transform3d transform) =
    transform.rotationX


toRotationY : Transform3d -> Float
toRotationY (Transform3d transform) =
    transform.rotationY


toRotationZ : Transform3d -> Float
toRotationZ (Transform3d transform) =
    transform.rotationZ
