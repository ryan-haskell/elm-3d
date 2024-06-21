module Elm3d.Frame exposing
    ( Frame
    , isKeyPressed
    , isLeftClickPressed, isMiddleClickPressed, isRightClickPressed
    , toInputAxis, toInputVector
    )

{-| This module allows you to access information every frame of your 3D program.

@docs Frame


# **Keyboard input**

@docs isKeyPressed


# **Mouse input**

@docs isLeftClickPressed, isMiddleClickPressed, isRightClickPressed


# **Movement Direction**

@docs toInputAxis, toInputVector

-}

import Elm3d.Input
import Elm3d.Input.Key exposing (Key)
import Elm3d.Vector2 exposing (Vector2)


{-| You can use this to find out how much time has elapsed since the last frame, get the current time, or
determine what inputs are currently being pressed.
-}
type alias Frame =
    { dt : Float
    , time : Float
    , input : Elm3d.Input.Model
    }


{-| Determines if the provided key is pressed.

    isJumping : Bool
    isJumping =
        Elm3d.Frame.isKeyPressed frame KEY_SPACE

-}
isKeyPressed : Key -> Frame -> Bool
isKeyPressed key frame =
    Elm3d.Input.isKeyPressed frame.input key


{-| Determines if the mouse left-click button is pressed

    isAttacking : Bool
    isAttacking =
        Elm3d.Frame.isLeftClickPressed frame

-}
isLeftClickPressed : Frame -> Bool
isLeftClickPressed frame =
    Elm3d.Input.isLeftClickPressed frame.input


{-| Determines if the mouse right-click button is pressed

    isRotatingCamera : Bool
    isRotatingCamera =
        Elm3d.Frame.isRightClickPressed frame

-}
isRightClickPressed : Frame -> Bool
isRightClickPressed frame =
    Elm3d.Input.isRightClickPressed frame.input


{-| Determines if the mouse middle-click button is pressed

    isClickingScrollwheel : Bool
    isClickingScrollwheel =
        Elm3d.Frame.isMiddleClickPressed frame

-}
isMiddleClickPressed : Frame -> Bool
isMiddleClickPressed frame =
    Elm3d.Input.isMiddleClickPressed frame.input


{-| Get a value from -1 to 1 to represent the current value of a key.

  - When the left key is pressed, you'll get -1

  - When the right key is pressed, you'll get 1

  - When both or neither key is pressed, you'll get 0

```elm
carMoveDirection : Float
carMoveDirection =
    Elm3d.Frame.toInputAxis frame
        ( KEY_ARROW_DOWN, KEY_ARROW_UP )
```

-}
toInputAxis : Frame -> ( Key, Key ) -> Float
toInputAxis frame ( left, right ) =
    if isKeyPressed right frame then
        if isKeyPressed left frame then
            0

        else
            1

    else if isKeyPressed left frame then
        -1

    else
        0


{-| Like [toInputAxis](#toInputAxis), but works in 2D space. Common when moving a player up, down, left, and right.

    playerMovementDirection : Vector2
    playerMovementDirection =
        Elm3d.Frame.toInputVector frame
            { x = ( KEY_ARROW_LEFT, KEY_ARROW_RIGHT )
            , y = ( KEY_ARROW_DOWN, KEY_ARROW_UP )
            }

-}
toInputVector :
    Frame
    ->
        { x : ( Key, Key )
        , y : ( Key, Key )
        }
    -> Vector2
toInputVector frame keys =
    let
        x =
            toInputAxis frame keys.x

        y =
            toInputAxis frame keys.y
    in
    if x == 0 && y == 0 then
        Elm3d.Vector2.zero

    else
        Elm3d.Vector2.new x y
            |> Elm3d.Vector2.normalize
