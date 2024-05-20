module Elm3d.Input.Event exposing (Event(..))

{-| This module allows you to respond to keyboard and mouse events. This is the argument provided with `Node.withOnInput`.


# **Events**

@docs Event

-}

import Elm3d.Input.Key exposing (Key)
import Elm3d.Input.Mouse exposing (Button)


{-| When using `Node.onInput`, these are all the kinds of input events that you can expect.
-}
type Event
    = KeyPressed Key
    | KeyReleased Key
    | MousePressed Button
    | MouseReleased Button
