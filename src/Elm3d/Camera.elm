module Elm3d.Camera exposing
    ( Camera
    , orthographic, perspective, isometric
    , withOnUpdate, withOnInput
    , withPosition, withRotation
    , withPositionX, withPositionY, withPositionZ
    , withRotationX, withRotationY, withRotationZ
    , moveX, moveY, moveZ
    , rotateX, rotateY, rotateZ
    , toMatrix4, toOffset, hasUpdateFunction, update, onInput, withFov, withSize, withNear, withFar, withAngle, withIsometricRotation, withDistance, withOffset
    , toIsometricProps
    )

{-|

@docs Camera
@docs orthographic, perspective, isometric

@docs withOnUpdate, withOnInput

@docs withPosition, withRotation
@docs withPositionX, withPositionY, withPositionZ
@docs withRotationX, withRotationY, withRotationZ
@docs moveX, moveY, moveZ
@docs rotateX, rotateY, rotateZ


### Elm3d.Camera.Perspective

docs withFov, withNear, withFar


### Elm3d.Camera.Orthographic

docs withSize, withNear, withFar


### Elm3d.Camera.Isometric

docs withFov, withSize, withNear, withFar, withAngle, withRotation, withDistance, withOffset


### Elm3d.Internals.Camera

@docs toMatrix4, toOffset, hasUpdateFunction, update, onInput, withFov, withSize, withNear, withFar, withAngle, withIsometricRotation, withDistance, withOffset

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
    Camera (Elm3d.Node.camera { projection = Orthographic props })


perspective :
    { fov : Float
    , near : Float
    , far : Float
    }
    -> Camera
perspective props =
    Camera (Elm3d.Node.camera { projection = Perspective props })


isometric :
    { size : Float
    , angle : Float
    , rotation : Float
    , distance : Float
    , near : Float
    , far : Float
    , offset : Vector2
    }
    -> Camera
isometric props =
    Camera (Elm3d.Node.camera { projection = Isometric props })


toIsometricProps : Camera -> Maybe Elm3d.Camera.Projection.IsometricProps
toIsometricProps (Camera node) =
    case Elm3d.Node.toCameraProjection node of
        Nothing ->
            Nothing

        Just (Isometric props) ->
            Just props

        Just (Orthographic props) ->
            Nothing

        Just (Perspective props) ->
            Nothing



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


withSize : Float -> Camera -> Camera
withSize props (Camera node) =
    updateProjection node
        (\projection ->
            case projection of
                Orthographic data ->
                    Orthographic { data | size = props }

                Isometric data ->
                    Isometric { data | size = props }

                Perspective _ ->
                    projection
        )


withFov : Float -> Camera -> Camera
withFov props (Camera node) =
    onlyForPerspective node (\data -> { data | fov = props })


withNear : Float -> Camera -> Camera
withNear props (Camera node) =
    updateProjection node
        (\projection ->
            case projection of
                Orthographic data ->
                    Orthographic { data | near = props }

                Perspective data ->
                    Perspective { data | near = props }

                Isometric data ->
                    Isometric { data | near = props }
        )


withFar : Float -> Camera -> Camera
withFar props (Camera node) =
    updateProjection node
        (\projection ->
            case projection of
                Orthographic data ->
                    Orthographic { data | far = props }

                Perspective data ->
                    Perspective { data | far = props }

                Isometric data ->
                    Isometric { data | far = props }
        )


withAngle : Float -> Camera -> Camera
withAngle props (Camera node) =
    onlyForIsometric node (\data -> { data | angle = props })


withIsometricRotation : Float -> Camera -> Camera
withIsometricRotation props (Camera node) =
    onlyForIsometric node (\data -> { data | rotation = props })


withDistance : Float -> Camera -> Camera
withDistance props (Camera node) =
    onlyForIsometric node (\data -> { data | distance = props })


withOffset : Vector2 -> Camera -> Camera
withOffset props (Camera node) =
    onlyForIsometric node (\data -> { data | offset = props })


toMatrix4 : ( Int, Int ) -> Camera -> Matrix4
toMatrix4 window (Camera node) =
    case Elm3d.Node.toCameraProjection node of
        Just projection ->
            case projection of
                Orthographic props ->
                    toOrthographicCamera window props
                        |> applyTransform node

                Isometric props ->
                    toOrthographicCamera window props
                        |> applyIsometricTransform props

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
                        |> applyTransform node

        Nothing ->
            Math.Matrix4.identity


toOrthographicCamera : ( Int, Int ) -> { props | size : Float, near : Float, far : Float } -> Matrix4
toOrthographicCamera window { size, near, far } =
    let
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


updateProjection : Elm3d.Node.Node -> (Projection -> Projection) -> Camera
updateProjection node toProjection =
    case Elm3d.Node.toCameraProjection node of
        Nothing ->
            Camera node

        Just projection ->
            Camera (Elm3d.Node.updateProjection (toProjection projection) node)


onlyForPerspective :
    Elm3d.Node.Node
    -> (Elm3d.Camera.Projection.PerspectiveProps -> Elm3d.Camera.Projection.PerspectiveProps)
    -> Camera
onlyForPerspective node toProps =
    updateProjection node
        (\projection ->
            case projection of
                Perspective data ->
                    Perspective (toProps data)

                _ ->
                    projection
        )


onlyForIsometric :
    Elm3d.Node.Node
    -> (Elm3d.Camera.Projection.IsometricProps -> Elm3d.Camera.Projection.IsometricProps)
    -> Camera
onlyForIsometric node toProps =
    updateProjection node
        (\projection ->
            case projection of
                Isometric data ->
                    Isometric (toProps data)

                _ ->
                    projection
        )



-- Transforming the camera


applyIsometricTransform : Elm3d.Camera.Projection.IsometricProps -> Matrix4 -> Matrix4
applyIsometricTransform props mat4 =
    let
        isometricVector : Vector2
        isometricVector =
            Elm3d.Isometric.toOffsetVector
                { angle = props.rotation
                , input = props.offset
                }

        ( dx, dz ) =
            ( Elm3d.Vector2.x isometricVector
            , Elm3d.Vector2.y isometricVector
            )
    in
    mat4
        |> Math.Matrix4.rotate props.angle Elm3d.Vector3.positiveX
        |> Math.Matrix4.rotate -props.rotation Elm3d.Vector3.positiveY
        |> Math.Matrix4.translate
            (if props.angle >= pi / 2 then
                Elm3d.Vector3.new 0 -props.distance 0

             else
                Elm3d.Vector3.new
                    (props.distance * sin props.rotation + dx)
                    (props.distance * tan props.angle)
                    (props.distance * cos props.rotation + dz)
                    |> Math.Vector3.negate
            )


applyTransform : Node -> Matrix4 -> Matrix4
applyTransform node mat4 =
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
    in
    mat4
        -- Scale has no effect on the camera
        |> Math.Matrix4.rotate -rx Elm3d.Vector3.positiveX
        |> Math.Matrix4.rotate -ry Elm3d.Vector3.positiveY
        |> Math.Matrix4.rotate -rz Elm3d.Vector3.positiveZ
        |> Math.Matrix4.translate3 -x -y -z


toOffset : Camera -> Vector2
toOffset (Camera node) =
    case Elm3d.Node.toCameraProjection node of
        Nothing ->
            Elm3d.Vector2.zero

        Just (Isometric props) ->
            props.offset

        Just (Orthographic _) ->
            Elm3d.Vector2.zero

        Just (Perspective _) ->
            Elm3d.Vector2.zero
