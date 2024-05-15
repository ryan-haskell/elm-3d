module Elm3d.Input.Event exposing (Event(..))

{-|

@docs Event

-}

import Elm3d.Input.Key exposing (Key)
import Elm3d.Input.Mouse exposing (Button)


{-| When using `Elm3d.Node.onInput`, these are all the kinds of events that can come in.
-}
type Event
    = KeyPressed Key
    | KeyReleased Key
    | MousePressed Button
    | MouseReleased Button
