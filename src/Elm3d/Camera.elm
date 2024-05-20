module Elm3d.Camera exposing
    ( Camera
    , orthographic, perspective, isometric
    , withPosition, withRotation
    , withPositionX, withPositionY, withPositionZ
    , withRotationX, withRotationY, withRotationZ
    , withOnFrame, withOnInput
    , map
    )

{-| This module allows you to define cameras that view your 3D world.


# **Creating cameras**

@docs Camera
@docs orthographic, perspective, isometric


# **Movement & rotation**

@docs withPosition, withRotation
@docs withPositionX, withPositionY, withPositionZ
@docs withRotationX, withRotationY, withRotationZ


# **Handling events**

@docs withOnFrame, withOnInput
@docs map

-}

import Elm3d.Camera.Projection exposing (Projection(..))
import Elm3d.Input.Event
import Elm3d.Internals.Camera as Camera exposing (Camera(..))
import Elm3d.Internals.Node as Node exposing (Node)
import Elm3d.Isometric
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector2 exposing (Vector2)
import Elm3d.Vector3 exposing (Vector3)
import Math.Matrix4
import Math.Vector3



-- CAMERA


type alias Camera msg =
    Camera.Camera msg



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
    , incline : Float
    , spin : Float
    , distance : Float
    , range : ( Float, Float )
    , offset : Vector2
    }
    -> Camera msg
isometric props =
    Camera (Node.camera { projection = Isometric props })



-- POSITION


withPosition : Vector3 -> Camera msg -> Camera msg
withPosition props (Camera node) =
    Camera (Node.withPosition props node)


withPositionX : Float -> Camera msg -> Camera msg
withPositionX props (Camera node) =
    Camera (Node.withPositionX props node)


withPositionY : Float -> Camera msg -> Camera msg
withPositionY props (Camera node) =
    Camera (Node.withPositionY props node)


withPositionZ : Float -> Camera msg -> Camera msg
withPositionZ props (Camera node) =
    Camera (Node.withPositionZ props node)



-- ROTATION


withRotation : Vector3 -> Camera msg -> Camera msg
withRotation props (Camera node) =
    Camera (Node.withRotation props node)


withRotationX : Float -> Camera msg -> Camera msg
withRotationX props (Camera node) =
    Camera (Node.withRotationX props node)


withRotationY : Float -> Camera msg -> Camera msg
withRotationY props (Camera node) =
    Camera (Node.withRotationY props node)


withRotationZ : Float -> Camera msg -> Camera msg
withRotationZ props (Camera node) =
    Camera (Node.withRotationZ props node)



-- EVENTS


map : (msg1 -> msg2) -> Camera msg1 -> Camera msg2
map fn (Camera node) =
    Camera (Node.map fn node)


withOnFrame : (Node.Context -> msg) -> Camera msg -> Camera msg
withOnFrame fn (Camera node) =
    Camera (Node.withOnFrame fn node)


withOnInput : (Elm3d.Input.Event.Event -> msg) -> Camera msg -> Camera msg
withOnInput fn (Camera node) =
    Camera (Node.withOnInput fn node)
