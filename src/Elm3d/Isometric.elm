module Elm3d.Isometric exposing (toOffsetVector)

import Elm3d.Vector2 exposing (Vector2)


toOffsetVector : { angle : Float, input : Vector2 } -> Vector2
toOffsetVector props =
    let
        input : { x : Float, y : Float }
        input =
            Elm3d.Vector2.toRecord props.input

        a =
            props.angle

        compA =
            pi / 2 - a
    in
    Elm3d.Vector2.new
        (input.x * cos a - input.y * cos compA)
        (-input.x * sin a - input.y * sin compA)
