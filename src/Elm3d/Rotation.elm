module Elm3d.Rotation exposing (lerp)

import Elm3d.Vector2 exposing (Vector2)
import Math.Vector2


lerp : { from : Float, to : Float, step : Float } -> Float
lerp { from, to, step } =
    let
        current =
            Elm3d.Vector2.new (cos from) (sin from)

        target =
            Elm3d.Vector2.new (cos to) (sin to)

        { x, y } =
            Math.Vector2.sub target current
                |> Math.Vector2.scale step
                |> Math.Vector2.add current
                |> Math.Vector2.toRecord
    in
    atan2 y x
