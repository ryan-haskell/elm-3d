module Elm3d.Matrix4 exposing
    ( Matrix4
    , identity
    , fromOrthoCamera, fromLookAt
    , withTranslate, withRotate, withScale
    )

{-|

@docs Matrix4

@docs identity
@docs fromOrthoCamera, fromLookAt

@docs withTranslate, withRotate, withScale

-}

import Elm3d.Vector3 exposing (Vector3)
import Math.Matrix4


type alias Matrix4 =
    Math.Matrix4.Mat4


identity : Matrix4
identity =
    Math.Matrix4.identity


fromLookAt :
    { eye : Vector3
    , target : Vector3
    , up : Vector3
    }
    -> Matrix4
fromLookAt props =
    Math.Matrix4.makeLookAt
        props.target
        props.eye
        props.up


fromOrthoCamera :
    { top : Float
    , bottom : Float
    , left : Float
    , right : Float
    , near : Float
    , far : Float
    }
    -> Matrix4
fromOrthoCamera props =
    Math.Matrix4.makeOrtho
        props.left
        props.right
        props.bottom
        props.top
        props.near
        props.far


withRotate : { radians : Float, axis : Vector3 } -> Matrix4 -> Matrix4
withRotate { radians, axis } mat4 =
    Math.Matrix4.rotate radians axis mat4


withTranslate : Vector3 -> Matrix4 -> Matrix4
withTranslate vec3 mat4 =
    Math.Matrix4.translate vec3 mat4


withScale : Vector3 -> Matrix4 -> Matrix4
withScale vec3 mat4 =
    Math.Matrix4.scale vec3 mat4
