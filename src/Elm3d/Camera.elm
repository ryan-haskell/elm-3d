module Elm3d.Camera exposing
    ( Camera
    , orthographic
    , perspective
    , toMatrix4
    , withOnUpdate
    , withPosition
    )

import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector3 exposing (Vector3)
import Math.Matrix4
import Math.Vector3



-- CAMERA


type Camera
    = Camera Internals


type alias Internals =
    { projection : Projection
    , position : Vector3
    , onUpdate : Maybe (Float -> Camera -> Camera)
    }


type Projection
    = Orthographic
        { size : Float
        , near : Float
        , far : Float
        }
    | Perspective
        { fov : Float
        , near : Float
        , far : Float
        }



-- | LookAt
--     { size : Float
--     , target : Vector3
--     }
-- CONSTRUCTORS


orthographic : { size : Float, near : Float, far : Float } -> Camera
orthographic props =
    Camera
        { projection = Orthographic props
        , position = Elm3d.Vector3.zero
        , onUpdate = Nothing
        }


perspective :
    { fov : Float
    , near : Float
    , far : Float
    }
    -> Camera
perspective props =
    Camera
        { projection = Perspective props
        , position = Elm3d.Vector3.zero
        , onUpdate = Nothing
        }



-- MODIFIERS


withPosition : Vector3 -> Camera -> Camera
withPosition props (Camera camera) =
    Camera { camera | position = props }


withOnUpdate : (Float -> Camera -> Camera) -> Camera -> Camera
withOnUpdate props (Camera camera) =
    Camera { camera | onUpdate = Just props }


toMatrix4 : ( Int, Int ) -> Camera -> Matrix4
toMatrix4 window (Camera camera) =
    case camera.projection of
        Orthographic { size, near, far } ->
            let
                ( width, height ) =
                    Tuple.mapBoth
                        Basics.toFloat
                        Basics.toFloat
                        window

                aspect =
                    width / height
            in
            Math.Matrix4.makeOrtho
                -(size / 2)
                (size / 2)
                -(size / aspect / 2)
                (size / aspect / 2)
                near
                far
                |> Math.Matrix4.translate (Math.Vector3.negate camera.position)

        Perspective { fov, near, far } ->
            let
                ( width, height ) =
                    Tuple.mapBoth
                        Basics.toFloat
                        Basics.toFloat
                        window

                aspect =
                    width / height
            in
            Math.Matrix4.makePerspective
                fov
                aspect
                near
                far
                |> Math.Matrix4.translate (Math.Vector3.negate camera.position)
