module Elm3d.Camera exposing
    ( Camera
    , orthographic, perspective, isometric
    , withOnFrame, withOnInput
    , withPosition, withRotation
    , withPositionX, withPositionY, withPositionZ
    , withRotationX, withRotationY, withRotationZ
    , moveX, moveY, moveZ
    , rotateX, rotateY, rotateZ
    , toMatrix4, toOffset, hasUpdateFunction, update, onInput, withFov, withSize, withNear, withFar, withAngle, withIsometricRotation, withDistance, withOffset
    , map, toIsometricProps
    )

{-|

@docs Camera
@docs orthographic, perspective, isometric

@docs withOnFrame, withOnInput

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
import Elm3d.Internals.Node as Node exposing (Node)
import Elm3d.Isometric
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector2 exposing (Vector2)
import Elm3d.Vector3 exposing (Vector3)
import Math.Matrix4
import Math.Vector3



-- CAMERA


type Camera msg
    = Camera (Node msg)


map : (a -> b) -> Camera a -> Camera b
map fn (Camera node) =
    Camera (Node.map fn node)



-- CONSTRUCTORS


orthographic :
    { size : Float
    , range : ( Float, Float )
    }
    -> Camera msg
orthographic props =
    Camera (Node.camera { projection = Orthographic props })


perspective :
    { fov : Float
    , range : ( Float, Float )
    }
    -> Camera msg
perspective props =
    Camera (Node.camera { projection = Perspective props })


isometric :
    { size : Float
    , angle : Float
    , rotation : Float
    , distance : Float
    , range : ( Float, Float )
    , offset : Vector2
    }
    -> Camera msg
isometric props =
    Camera (Node.camera { projection = Isometric props })


toIsometricProps : Camera msg -> Maybe Elm3d.Camera.Projection.IsometricProps
toIsometricProps (Camera node) =
    case Node.toCameraProjection node of
        Nothing ->
            Nothing

        Just (Isometric props) ->
            Just props

        Just (Orthographic props) ->
            Nothing

        Just (Perspective props) ->
            Nothing



-- MODIFIERS


withPosition : Vector3 -> Camera msg -> Camera msg
withPosition props (Camera node) =
    Camera (Node.withPosition props node)


withRotation : Vector3 -> Camera msg -> Camera msg
withRotation props (Camera node) =
    Camera (Node.withRotation props node)


withPositionX : Float -> Camera msg -> Camera msg
withPositionX props (Camera node) =
    Camera (Node.withPositionX props node)


withPositionY : Float -> Camera msg -> Camera msg
withPositionY props (Camera node) =
    Camera (Node.withPositionY props node)


withPositionZ : Float -> Camera msg -> Camera msg
withPositionZ props (Camera node) =
    Camera (Node.withPositionZ props node)


withRotationX : Float -> Camera msg -> Camera msg
withRotationX props (Camera node) =
    Camera (Node.withRotationX props node)


withRotationY : Float -> Camera msg -> Camera msg
withRotationY props (Camera node) =
    Camera (Node.withRotationY props node)


withRotationZ : Float -> Camera msg -> Camera msg
withRotationZ props (Camera node) =
    Camera (Node.withRotationZ props node)


moveX : Float -> Camera msg -> Camera msg
moveX props (Camera node) =
    Camera (Node.moveX props node)


moveY : Float -> Camera msg -> Camera msg
moveY props (Camera node) =
    Camera (Node.moveY props node)


moveZ : Float -> Camera msg -> Camera msg
moveZ props (Camera node) =
    Camera (Node.moveZ props node)


rotateX : Float -> Camera msg -> Camera msg
rotateX props (Camera node) =
    Camera (Node.rotateX props node)


rotateY : Float -> Camera msg -> Camera msg
rotateY props (Camera node) =
    Camera (Node.rotateY props node)


rotateZ : Float -> Camera msg -> Camera msg
rotateZ props (Camera node) =
    Camera (Node.rotateZ props node)


hasUpdateFunction : Camera msg -> Bool
hasUpdateFunction (Camera node) =
    Node.hasUpdateFunction node


update : Node.Context -> Camera msg -> List msg
update ctx (Camera node) =
    Node.update ctx node


onInput : Elm3d.Input.Event.Event -> Camera msg -> List msg
onInput ctx (Camera node) =
    Node.onInput ctx node


withOnFrame : (Node.Context -> msg) -> Camera msg -> Camera msg
withOnFrame fn (Camera node) =
    Camera (Node.withOnFrame fn node)


withOnInput : (Elm3d.Input.Event.Event -> msg) -> Camera msg -> Camera msg
withOnInput fn (Camera node) =
    Camera (Node.withOnInput fn node)


withSize : Float -> Camera msg -> Camera msg
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


withFov : Float -> Camera msg -> Camera msg
withFov props (Camera node) =
    onlyForPerspective node (\data -> { data | fov = props })


withNear : Float -> Camera msg -> Camera msg
withNear props (Camera node) =
    updateProjection node
        (\projection ->
            case projection of
                Orthographic data ->
                    Orthographic { data | range = Tuple.mapFirst (\_ -> props) data.range }

                Perspective data ->
                    Perspective { data | range = Tuple.mapFirst (\_ -> props) data.range }

                Isometric data ->
                    Isometric { data | range = Tuple.mapFirst (\_ -> props) data.range }
        )


withFar : Float -> Camera msg -> Camera msg
withFar props (Camera node) =
    updateProjection node
        (\projection ->
            case projection of
                Orthographic data ->
                    Orthographic { data | range = Tuple.mapSecond (\_ -> props) data.range }

                Perspective data ->
                    Perspective { data | range = Tuple.mapSecond (\_ -> props) data.range }

                Isometric data ->
                    Isometric { data | range = Tuple.mapSecond (\_ -> props) data.range }
        )


withAngle : Float -> Camera msg -> Camera msg
withAngle props (Camera node) =
    onlyForIsometric node (\data -> { data | angle = props })


withIsometricRotation : Float -> Camera msg -> Camera msg
withIsometricRotation props (Camera node) =
    onlyForIsometric node (\data -> { data | rotation = props })


withDistance : Float -> Camera msg -> Camera msg
withDistance props (Camera node) =
    onlyForIsometric node (\data -> { data | distance = props })


withOffset : Vector2 -> Camera msg -> Camera msg
withOffset props (Camera node) =
    onlyForIsometric node (\data -> { data | offset = props })


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


updateProjection : Node.Node msg -> (Projection -> Projection) -> Camera msg
updateProjection node toProjection =
    case Node.toCameraProjection node of
        Nothing ->
            Camera node

        Just projection ->
            Camera (Node.updateProjection (toProjection projection) node)


onlyForPerspective :
    Node.Node msg
    -> (Elm3d.Camera.Projection.PerspectiveProps -> Elm3d.Camera.Projection.PerspectiveProps)
    -> Camera msg
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
    Node.Node msg
    -> (Elm3d.Camera.Projection.IsometricProps -> Elm3d.Camera.Projection.IsometricProps)
    -> Camera msg
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


toOffset : Camera msg -> Vector2
toOffset (Camera node) =
    case Node.toCameraProjection node of
        Nothing ->
            Elm3d.Vector2.zero

        Just (Isometric props) ->
            props.offset

        Just (Orthographic _) ->
            Elm3d.Vector2.zero

        Just (Perspective _) ->
            Elm3d.Vector2.zero
