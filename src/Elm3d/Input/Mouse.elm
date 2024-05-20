module Elm3d.Input.Mouse exposing (Button(..))

{-| This module provides mouse values like `Button` as custom types.


# **Mouse buttons**

@docs Button

-}


{-| Represents which mouse button that was pressed, released, etc
-}
type Button
    = LeftClick
    | MiddleClick
    | RightClick
