module Elm3d.Input.Event exposing (Event(..))

{-|

@docs Event

-}

import Elm3d.Input.Key exposing (Key)


{-| When using `Elm3d.Node.onInput`, these are all the kinds of events that can come in.
-}
type Event
    = KeyPressed Key
    | KeyReleased Key
