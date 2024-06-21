module Elm3d.Camera exposing
    ( Camera
    , orthographic, perspective, isometric
    , withPosition
    , withPositionX, withPositionY, withPositionZ
    , withRotation
    , withRotationX, withRotationY, withRotationZ
    , withOnFrame, withOnInput
    , map
    )

{-| This module allows you to define cameras that view your 3D world.


# **Creating cameras**

@docs Camera
@docs orthographic, perspective, isometric


# **Moving and rotating**

@docs withPosition
@docs withPositionX, withPositionY, withPositionZ

@docs withRotation
@docs withRotationX, withRotationY, withRotationZ


# **Handling events**

@docs withOnFrame, withOnInput
@docs map

-}

import Elm3d.Camera.Projection exposing (Projection(..))
import Elm3d.Frame exposing (Frame)
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


{-| Cameras allow you to control how users see your 3D world.
-}
type alias Camera msg =
    Camera.Camera msg



-- CONSTRUCTORS


{-| Create a camera with an orthographic projection.

    camera : Camera msg
    camera =
        orthographic
            { size = 10
            , range = ( 1, 100 )
            }

  - **`size`** - How many units wide this camera can see (height is calculated by viewport ratio)
  - **`range`** - The near and far limit for visiblity (anything within range of the camera will be visible)

-}
orthographic :
    { size : Float
    , range : ( Float, Float )
    }
    -> Camera msg
orthographic props =
    Camera (Node.camera { projection = Orthographic props })


{-| Create a camera with a perspective projection.

    camera : Camera msg
    camera =
        perspective
            { fov = 60
            , range = ( 1, 100 )
            }

  - **`fov`** - The field-of-view for this camera, in degrees
  - **`range`** - The near and far limit for visiblity (anything within range of the camera will be visible)

-}
perspective :
    { fov : Float
    , range : ( Float, Float )
    }
    -> Camera msg
perspective props =
    Camera (Node.camera { projection = Perspective props })


{-| A specialized orthographic camera designed for making isometric 3D scenes.

    import Elm3d.Vector2

    camera : Camera msg
    camera =
        isometric
            { size = 10
            , incline = pi / 4
            , spin = pi / 4
            , distance = 50
            , range = ( 1, 100 )
            , offset = Elm3d.Vector2.zero
            }

  - **`size`** - How many units wide this camera can see (height is calculated by viewport ratio)
  - **`range`** - The near and far limit for visiblity (anything within range of the camera will be visible)
  - **`incline`** - The vertical angle for the camera, in radians
  - **`spin`** - The horizontal angle for the camera, in radians
  - **`distance`** - How far the camera is from the view target (easier than working with position)
  - **`offset`** - The XZ position of the camera (easier than working with position)

-}
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


{-| Set this camera's position using a 3D vector.

    import Elm3d.Vector3 exposing (Vector3)

    camera : Camera msg
    camera =
        orthographic
            { size = 10
            , range = ( 1, 100 )
            }
            |> withPositionX (Elm3d.Vector3.new 0 10 10)

-}
withPosition : Vector3 -> Camera msg -> Camera msg
withPosition props (Camera node) =
    Camera (Node.withPosition props node)


{-| Set this camera's position along the **x-axis**.

    camera : Camera msg
    camera =
        orthographic
            { size = 10
            , range = ( 1, 100 )
            }
            |> withPositionX 5

-}
withPositionX : Float -> Camera msg -> Camera msg
withPositionX props (Camera node) =
    Camera (Node.withPositionX props node)


{-| Set this camera's position along the **y-axis**.

    camera : Camera msg
    camera =
        orthographic
            { size = 10
            , range = ( 1, 100 )
            }
            |> withPositionY 5

-}
withPositionY : Float -> Camera msg -> Camera msg
withPositionY props (Camera node) =
    Camera (Node.withPositionY props node)


{-| Set this camera's position along the **z-axis**.

    camera : Camera msg
    camera =
        orthographic
            { size = 10
            , range = ( 1, 100 )
            }
            |> withPositionZ 5

-}
withPositionZ : Float -> Camera msg -> Camera msg
withPositionZ props (Camera node) =
    Camera (Node.withPositionZ props node)



-- ROTATION


{-| Set this camera's rotation using a 3D vector

    import Elm3d.Vector3 exposing (Vector3)

    camera : Camera msg
    camera =
        orthographic
            { size = 10
            , range = ( 1, 100 )
            }
            |> withRotation (Elm3d.Vector3.new 5 0 2)

-}
withRotation : Vector3 -> Camera msg -> Camera msg
withRotation props (Camera node) =
    Camera (Node.withRotation props node)


{-| Rotate the camera around the **x-axis**

    camera : Camera msg
    camera =
        orthographic
            { size = 10
            , range = ( 1, 100 )
            }
            |> withRotationX 2

-}
withRotationX : Float -> Camera msg -> Camera msg
withRotationX props (Camera node) =
    Camera (Node.withRotationX props node)


{-| Rotate the camera around the **y-axis**

    camera : Camera msg
    camera =
        orthographic
            { size = 10
            , range = ( 1, 100 )
            }
            |> withRotationY 2

-}
withRotationY : Float -> Camera msg -> Camera msg
withRotationY props (Camera node) =
    Camera (Node.withRotationY props node)


{-| Rotate the camera around the **z-axis**

    camera : Camera msg
    camera =
        orthographic
            { size = 10
            , range = ( 1, 100 )
            }
            |> withRotationZ 2

-}
withRotationZ : Float -> Camera msg -> Camera msg
withRotationZ props (Camera node) =
    Camera (Node.withRotationZ props node)



-- EVENTS


{-| Provide a message to call every frame.

    import Elm3d.Frame exposing (Frame)

    type Msg
        = CameraOnFrame Frame

    camera : Camera Msg
    camera =
        orthographic
            { size = 10
            , range = ( 1, 100 )
            }
            |> withOnFrame CameraOnFrame

-}
withOnFrame : (Frame -> msg) -> Camera msg -> Camera msg
withOnFrame fn (Camera node) =
    Camera (Node.withOnFrame fn node)


{-| Provide a message to call for any input event.

    import Elm3d.Input.Event exposing (Event)

    type Msg
        = CameraOnInput Event

    camera : Camera Msg
    camera =
        orthographic
            { size = 10
            , range = ( 1, 100 )
            }
            |> withOnInput CameraOnInput

-}
withOnInput : (Elm3d.Input.Event.Event -> msg) -> Camera msg -> Camera msg
withOnInput fn (Camera node) =
    Camera (Node.withOnInput fn node)


{-| Convert a camera with one `msg` type into another. This is commonly needed when nesting
the Elm Architecture with a page or component:

    import Pages.Dashboard

    type Msg
        = Dashboard Pages.Dashboard.Msg

    camera : Camera Pages.Dashboard.Msg
    camera =
        ...

    newCamera : Camera Msg
    newCamera =
        map Dashboard camera

-}
map : (msg1 -> msg2) -> Camera msg1 -> Camera msg2
map fn (Camera node) =
    Camera (Node.map fn node)
