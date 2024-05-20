module Elm3d.Float exposing (lerp)

{-| This module defines any convenience functions for working with floats in the context of 3D rendering. (There's just one for now!)

@docs lerp

-}


{-| Linearly interpolate from one float to another, by the given step size.

    lerp { from = 0, to = 10, step = 0 } == 0

    lerp { from = 0, to = 10, step = 0.5 } == 5

    lerp { from = 0, to = 10, step = 1 } == 10

    lerp { from = 0, to = -4, step = 0.25 } == -1

This is a nice way to animate a numeric value each frame when working with `Elm3d.Node.withOnFrame`.

    newZoom : Float
    newZoom =
        Elm3d.Float.lerp
            { from = model.zoom
            , to = model.targetZoom
            , step = dt * zoomSpeed
            }

**Working with angles?** Use [Elm3d.Angle.lerp](./Elm3d-Angle#lerp) instead! It prevents crazy stuff happening when animating across the `0` / `2π` threshold. Unless you like unpredictable 360-no-scope spins- then definitely use this lerp!

-}
lerp :
    { from : Float
    , to : Float
    , step : Float
    }
    -> Float
lerp { from, to, step } =
    (to - from) * clamp 0 1 step + from
