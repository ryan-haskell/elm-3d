module Elm3d.Isometric exposing (toInputVector)

{-| This module provides some common functions when working with an isometric camera.


# **User input**

@docs toInputVector

-}

import Elm3d.Vector2 exposing (Vector2)


{-| Depending on the angle of your isometric camera, a player's directional input (WASD or arrow keys) might not map to up/down/left/right on the screen.

This function takes in the horizontal angle your isometric camera is using to rotate the players input by the correct amount

    import Elm3d.Camera
    import Elm3d.Context exposing (Context)

    cameraSpin : Float
    cameraSpin =
        pi / 4

    camera : Camera msg
    camera =
        Elm3d.Camera.isometric
            { ...
            , spin = cameraSpin
            }

    toMovementDelta : Context -> Vector2
    toMovementDelta ctx =
        toInputVector
            { spin = cameraSpin
            , input =
                Elm3d.Context.toInputVector ctx
                    { x = ( KEY_ARROW_LEFT, KEY_ARROW_RIGHT )
                    , y = ( KEY_ARROW_DOWN, KEY_ARROW_UP )
                    }
            }

-}
toInputVector : { spin : Float, input : Vector2 } -> Vector2
toInputVector { spin, input } =
    let
        { x, y } =
            Elm3d.Vector2.toRecord input

        complement =
            pi / 2 - spin
    in
    Elm3d.Vector2.new
        (x * cos spin - y * cos complement)
        (-x * sin spin - y * sin complement)
