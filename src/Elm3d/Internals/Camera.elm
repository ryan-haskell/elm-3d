module Elm3d.Internals.Camera exposing (..)

import Elm3d.Camera.Projection exposing (..)
import Elm3d.Context exposing (Context)
import Elm3d.Input.Event
import Elm3d.Internals.Node as Node exposing (Node)
import Elm3d.Isometric
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector2 exposing (Vector2)
import Elm3d.Vector3 exposing (Vector3)
import Math.Matrix4
import Math.Vector3


type Camera msg
    = Camera (Node msg)


hasUpdateFunction : Camera msg -> Bool
hasUpdateFunction (Camera node) =
    Node.hasUpdateFunction node


update : Context -> Camera msg -> List msg
update ctx (Camera node) =
    Node.update ctx node


onInput : Elm3d.Input.Event.Event -> Camera msg -> List msg
onInput ctx (Camera node) =
    Node.onInput ctx node


toMatrix4 : ( Int, Int ) -> Camera msg -> Matrix4
toMatrix4 window (Camera node) =
    case Node.toCameraProjection node of
        Just projection ->
            case projection of
                Orthographic props ->
                    toOrthographicCamera window props
                        |> applyTransform node

                Isometric props ->
                    toOrthographicCamera window props
                        |> applyIsometricTransform props

                Perspective { fov, range } ->
                    let
                        ( near, far ) =
                            range

                        ( width, height ) =
                            Tuple.mapBoth
                                Basics.toFloat
                                Basics.toFloat
                                window

                        aspect =
                            width / height
                    in
                    Math.Matrix4.makePerspective fov aspect near far
                        |> applyTransform node

        Nothing ->
            Math.Matrix4.identity


toOrthographicCamera : ( Int, Int ) -> { props | size : Float, range : ( Float, Float ) } -> Matrix4
toOrthographicCamera window { size, range } =
    let
        ( near, far ) =
            range

        ( width, height ) =
            Tuple.mapBoth Basics.toFloat Basics.toFloat window

        half =
            size / 2

        aspect =
            width / height
    in
    Math.Matrix4.makeOrtho
        -half
        half
        (-half / aspect)
        (half / aspect)
        near
        far



-- Transforming the camera


applyIsometricTransform : Elm3d.Camera.Projection.IsometricProps -> Matrix4 -> Matrix4
applyIsometricTransform props mat4 =
    let
        isometricVector : Vector2
        isometricVector =
            Elm3d.Isometric.toInputVector
                { spin = props.spin
                , input = props.offset
                }

        ( dx, dz ) =
            ( Elm3d.Vector2.x isometricVector
            , Elm3d.Vector2.y isometricVector
            )
    in
    mat4
        |> Math.Matrix4.rotate props.incline Elm3d.Vector3.positiveX
        |> Math.Matrix4.rotate -props.spin Elm3d.Vector3.positiveY
        |> Math.Matrix4.translate
            (if props.incline >= pi / 2 then
                Elm3d.Vector3.new 0 -props.distance 0

             else
                Elm3d.Vector3.new
                    (props.distance * sin props.spin + dx)
                    (props.distance * tan props.incline)
                    (props.distance * cos props.spin + dz)
                    |> Math.Vector3.negate
            )


applyTransform : Node msg -> Matrix4 -> Matrix4
applyTransform node mat4 =
    let
        transform : Transform3d
        transform =
            Node.toTransform3d node

        { x, y, z } =
            Elm3d.Transform3d.toPosition transform
                |> Math.Vector3.toRecord

        rx =
            Elm3d.Transform3d.toRotationX transform

        ry =
            Elm3d.Transform3d.toRotationY transform

        rz =
            Elm3d.Transform3d.toRotationZ transform
    in
    mat4
        -- Scale has no effect on the camera
        |> Math.Matrix4.rotate -rx Elm3d.Vector3.positiveX
        |> Math.Matrix4.rotate -ry Elm3d.Vector3.positiveY
        |> Math.Matrix4.rotate -rz Elm3d.Vector3.positiveZ
        |> Math.Matrix4.translate3 -x -y -z
