module Elm3d.Camera exposing
    ( Camera
    , orthographic, perspective
    , withIsometricTransform
    , withOnUpdate, withOnInput
    , withPosition, withRotation
    , withPositionX, withPositionY, withPositionZ
    , withRotationX, withRotationY, withRotationZ
    , moveX, moveY, moveZ
    , rotateX, rotateY, rotateZ
    , toMatrix4, hasUpdateFunction, update, onInput
    )

{-|

@docs Camera
@docs orthographic, perspective
@docs withIsometricTransform

@docs withOnUpdate, withOnInput

@docs withPosition, withRotation
@docs withPositionX, withPositionY, withPositionZ
@docs withRotationX, withRotationY, withRotationZ
@docs moveX, moveY, moveZ
@docs rotateX, rotateY, rotateZ


### Internals

@docs toMatrix4, hasUpdateFunction, update, onInput

-}

import Elm3d.Camera.Projection exposing (Projection(..))
import Elm3d.Input.Event
import Elm3d.Isometric
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Node exposing (Node)
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector2 exposing (Vector2)
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



-- ISOMETRIC


withIsometricTransform :
    { horizontalAngle : Float
    , verticalAngle : Float
    , distance : Float
    , offset : Elm3d.Vector2.Vector2
    }
    -> Camera
    -> Camera
withIsometricTransform props camera =
    let
        isometricVector : Vector2
        isometricVector =
            Elm3d.Isometric.toOffsetVector
                { angle = props.horizontalAngle
                , input = props.offset
                }

        ( dx, dz ) =
            ( Elm3d.Vector2.x isometricVector
            , Elm3d.Vector2.y isometricVector
            )
    in
    camera
        |> withRotationX -props.verticalAngle
        |> withRotationY props.horizontalAngle
        |> withPosition
            (if props.verticalAngle >= pi / 2 then
                Elm3d.Vector3.new 0 props.distance 0

             else
                Elm3d.Vector3.new
                    (props.distance * sin props.horizontalAngle + dx)
                    (props.distance * tan props.verticalAngle)
                    (props.distance * cos props.horizontalAngle + dz)
            )



-- MODIFIERS


withPosition : Vector3 -> Camera -> Camera
withPosition props (Camera node) =
    Camera (Elm3d.Node.withPosition props node)


withRotation : Vector3 -> Camera -> Camera
withRotation props (Camera node) =
    Camera (Elm3d.Node.withRotation props node)


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


moveX : Float -> Camera -> Camera
moveX props (Camera node) =
    Camera (Elm3d.Node.moveX props node)


moveY : Float -> Camera -> Camera
moveY props (Camera node) =
    Camera (Elm3d.Node.moveY props node)


moveZ : Float -> Camera -> Camera
moveZ props (Camera node) =
    Camera (Elm3d.Node.moveZ props node)


rotateX : Float -> Camera -> Camera
rotateX props (Camera node) =
    Camera (Elm3d.Node.rotateX props node)


rotateY : Float -> Camera -> Camera
rotateY props (Camera node) =
    Camera (Elm3d.Node.rotateY props node)


rotateZ : Float -> Camera -> Camera
rotateZ props (Camera node) =
    Camera (Elm3d.Node.rotateZ props node)


hasUpdateFunction : Camera -> Bool
hasUpdateFunction (Camera node) =
    Elm3d.Node.hasUpdateFunction node


update : Elm3d.Node.Context -> Camera -> Camera
update ctx (Camera node) =
    Camera (Elm3d.Node.update ctx node)


onInput : Elm3d.Input.Event.Event -> Camera -> Camera
onInput ctx (Camera node) =
    Camera (Elm3d.Node.onInput ctx node)


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


withOnInput : (Elm3d.Input.Event.Event -> Camera -> Camera) -> Camera -> Camera
withOnInput fn (Camera node) =
    Camera
        (Elm3d.Node.withOnInput
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
                        -- Scale has no effect on the camera
                        |> Math.Matrix4.rotate -rx Elm3d.Vector3.positiveX
                        |> Math.Matrix4.rotate -ry Elm3d.Vector3.positiveY
                        |> Math.Matrix4.rotate -rz Elm3d.Vector3.positiveZ
                        |> Math.Matrix4.translate3 -x -y -z

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
                        -- Scale has no effect on the camera
                        |> Math.Matrix4.rotate -rx Elm3d.Vector3.positiveX
                        |> Math.Matrix4.rotate -ry Elm3d.Vector3.positiveY
                        |> Math.Matrix4.rotate -rz Elm3d.Vector3.positiveZ
                        |> Math.Matrix4.translate3 -x -y -z

        Nothing ->
            Math.Matrix4.identity
