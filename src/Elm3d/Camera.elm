module Elm3d.Camera exposing
    ( Camera
    , orthographic, perspective
    , withOnUpdate
    , withPosition, withRotation, withScale
    , withPositionX, withPositionY, withPositionZ
    , withRotationX, withRotationY, withRotationZ
    , withScaleX, withScaleY, withScaleZ
    , toMatrix4, hasUpdateFunction, update
    )

{-|

@docs Camera
@docs orthographic, perspective

@docs withOnUpdate, withOnInput

@docs withPosition, withRotation, withScale
@docs withPositionX, withPositionY, withPositionZ
@docs withRotationX, withRotationY, withRotationZ
@docs withScaleX, withScaleY, withScaleZ


### Internals

@docs toMatrix4, hasUpdateFunction, update

-}

import Elm3d.Camera.Projection exposing (Projection(..))
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Node exposing (Node)
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector3 exposing (Vector3)
import Math.Matrix4
import Math.Vector3



-- CAMERA


type Camera
    = Camera Node



-- CONSTRUCTORS


orthographic :
    { size : Float
    , near : Float
    , far : Float
    }
    -> Camera
orthographic props =
    Camera
        (Elm3d.Node.camera
            { projection = Orthographic props
            }
        )


perspective :
    { fov : Float
    , near : Float
    , far : Float
    }
    -> Camera
perspective props =
    Camera
        (Elm3d.Node.camera
            { projection = Perspective props
            }
        )



-- MODIFIERS


withPosition : Vector3 -> Camera -> Camera
withPosition props (Camera node) =
    Camera (Elm3d.Node.withPosition props node)


withRotation : Vector3 -> Camera -> Camera
withRotation props (Camera node) =
    Camera (Elm3d.Node.withRotation props node)


withScale : Vector3 -> Camera -> Camera
withScale props (Camera node) =
    Camera (Elm3d.Node.withScale props node)


withPositionX : Float -> Camera -> Camera
withPositionX props (Camera node) =
    Camera (Elm3d.Node.withPositionX props node)


withPositionY : Float -> Camera -> Camera
withPositionY props (Camera node) =
    Camera (Elm3d.Node.withPositionY props node)


withPositionZ : Float -> Camera -> Camera
withPositionZ props (Camera node) =
    Camera (Elm3d.Node.withPositionZ props node)


withRotationX : Float -> Camera -> Camera
withRotationX props (Camera node) =
    Camera (Elm3d.Node.withRotationX props node)


withRotationY : Float -> Camera -> Camera
withRotationY props (Camera node) =
    Camera (Elm3d.Node.withRotationY props node)


withRotationZ : Float -> Camera -> Camera
withRotationZ props (Camera node) =
    Camera (Elm3d.Node.withRotationZ props node)


withScaleX : Float -> Camera -> Camera
withScaleX props (Camera node) =
    Camera (Elm3d.Node.withScaleX props node)


withScaleY : Float -> Camera -> Camera
withScaleY props (Camera node) =
    Camera (Elm3d.Node.withScaleY props node)


withScaleZ : Float -> Camera -> Camera
withScaleZ props (Camera node) =
    Camera (Elm3d.Node.withScaleZ props node)


hasUpdateFunction : Camera -> Bool
hasUpdateFunction (Camera node) =
    Elm3d.Node.hasUpdateFunction node


update : Elm3d.Node.Context -> Camera -> Camera
update ctx (Camera node) =
    Camera (Elm3d.Node.update ctx node)


withOnUpdate : (Elm3d.Node.Context -> Camera -> Camera) -> Camera -> Camera
withOnUpdate fn (Camera node) =
    Camera
        (Elm3d.Node.withOnUpdate
            (\ctx nodeIn ->
                let
                    (Camera nodeOut) =
                        fn ctx (Camera nodeIn)
                in
                nodeOut
            )
            node
        )


toMatrix4 : ( Int, Int ) -> Camera -> Matrix4
toMatrix4 window (Camera node) =
    case Elm3d.Node.toCameraProjection node of
        Just projection ->
            let
                transform : Transform3d
                transform =
                    Elm3d.Node.toTransform3d node

                { x, y, z } =
                    Elm3d.Transform3d.toPosition transform
                        |> Math.Vector3.toRecord

                rx =
                    Elm3d.Transform3d.toRotationX transform

                ry =
                    Elm3d.Transform3d.toRotationY transform

                rz =
                    Elm3d.Transform3d.toRotationZ transform

                applyTransform : Matrix4 -> Matrix4
                applyTransform mat4 =
                    mat4
                        -- Scale has no effect on the camera
                        |> Math.Matrix4.rotate -rx Elm3d.Vector3.positiveX
                        |> Math.Matrix4.rotate -ry Elm3d.Vector3.positiveY
                        |> Math.Matrix4.rotate -rz Elm3d.Vector3.positiveZ
                        |> Math.Matrix4.translate3 -x -y -z
            in
            case projection of
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
                        |> applyTransform

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
                    Math.Matrix4.makePerspective fov aspect near far
                        |> applyTransform

        Nothing ->
            Math.Matrix4.identity
