module Elm3d.Angle exposing (lerp)

{-| This module defines any convenience functions for working with
angles in the context of 3D rendering.

@docs lerp

-}

import Elm3d.Vector2 exposing (Vector2)
import Math.Vector2


{-| Linearly interpolate from one angle to another, without issues when
crossing across the `2π` to `0` boundary.

    lerp { from = 0, to = pi, step = 0 } == 0

    lerp { from = 0, to = pi, step = 0.5 } == pi / 2

    lerp { from = 0, to = pi, step = 1 } == pi

    lerp { from = 2 * pi, to = pi / 2, step = 0.5 } == pi / 4

This is a nice way to change something's rotation on each frame when working with `Elm3d.Node.withOnFrame`.

    cameraSpin : Float
    cameraSpin =
        Elm3d.Float.lerp
            { from = model.cameraSpin
            , to = model.playerRotation
            , step = dt * 10.0
            }

**Not working with angles?** Use [Elm3d.Float.lerp](./Elm3d-Float#lerp) instead! It won't be limited to values ranging from `0` to `2π`

-}
lerp : { from : Float, to : Float, step : Float } -> Float
lerp { from, to, step } =
    let
        areOppositeDirections : Bool
        areOppositeDirections =
            abs (abs (from - to) - pi) <= 0.01
    in
    if areOppositeDirections then
        from + step * pi

    else
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
