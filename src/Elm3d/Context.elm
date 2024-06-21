module Elm3d.Context exposing
    ( Context
    , isKeyPressed
    , isLeftClickPressed, isMiddleClickPressed, isRightClickPressed
    , toInputAxis, toInputVector
    )

{-| This module allows you to access information every frame of your 3D program.

@docs Context


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
type alias Context =
    { dt : Float
    , time : Float
    , input : Elm3d.Input.Model
    }


{-| Determines if the provided key is pressed.

    isJumping : Bool
    isJumping =
        Elm3d.Context.isKeyPressed ctx KEY_SPACE

-}
isKeyPressed : Key -> Context -> Bool
isKeyPressed key ctx =
    Elm3d.Input.isKeyPressed ctx.input key


{-| Determines if the mouse left-click button is pressed

    isAttacking : Bool
    isAttacking =
        Elm3d.Context.isLeftClickPressed ctx

-}
isLeftClickPressed : Context -> Bool
isLeftClickPressed ctx =
    Elm3d.Input.isLeftClickPressed ctx.input


{-| Determines if the mouse right-click button is pressed

    isRotatingCamera : Bool
    isRotatingCamera =
        Elm3d.Context.isRightClickPressed ctx

-}
isRightClickPressed : Context -> Bool
isRightClickPressed ctx =
    Elm3d.Input.isRightClickPressed ctx.input


{-| Determines if the mouse middle-click button is pressed

    isClickingScrollwheel : Bool
    isClickingScrollwheel =
        Elm3d.Context.isMiddleClickPressed ctx

-}
isMiddleClickPressed : Context -> Bool
isMiddleClickPressed ctx =
    Elm3d.Input.isMiddleClickPressed ctx.input


{-| Get a value from -1 to 1 to represent the current value of a key.

  - When the left key is pressed, you'll get -1

  - When the right key is pressed, you'll get 1

  - When both or neither key is pressed, you'll get 0

```elm
carMoveDirection : Float
carMoveDirection =
    Elm3d.Context.toInputAxis ctx
        ( KEY_ARROW_DOWN, KEY_ARROW_UP )
```

-}
toInputAxis : Context -> ( Key, Key ) -> Float
toInputAxis ctx ( left, right ) =
    if isKeyPressed right ctx then
        if isKeyPressed left ctx then
            0

        else
            1

    else if isKeyPressed left ctx then
        -1

    else
        0


{-| Like [toInputAxis](#toInputAxis), but works in 2D space. Common when moving a player up, down, left, and right.

    playerMovementDirection : Vector2
    playerMovementDirection =
        Elm3d.Context.toInputVector ctx
            { x = ( KEY_ARROW_LEFT, KEY_ARROW_RIGHT )
            , y = ( KEY_ARROW_DOWN, KEY_ARROW_UP )
            }

-}
toInputVector :
    Context
    ->
        { x : ( Key, Key )
        , y : ( Key, Key )
        }
    -> Vector2
toInputVector ctx keys =
    let
        x =
            toInputAxis ctx keys.x

        y =
            toInputAxis ctx keys.y
    in
    if x == 0 && y == 0 then
        Elm3d.Vector2.zero

    else
        Elm3d.Vector2.new x y
            |> Elm3d.Vector2.normalize
