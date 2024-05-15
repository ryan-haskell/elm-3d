module Elm3d.Camera.Isometric exposing (Camera, move, new, size)

import Elm3d.Camera
import Elm3d.Isometric
import Elm3d.Vector2 exposing (Vector2)
import Math.Vector2


type alias Camera model msg =
    Elm3d.Camera.Camera model msg


new :
    { size : Float
    , angle : Float
    , rotation : Float
    , distance : Float
    , near : Float
    , far : Float
    , offset : Vector2
    }
    -> Camera model msg
new props =
    Elm3d.Camera.isometric props


move : Vector2 -> Camera model msg -> Camera model msg
move input cam =
    case Elm3d.Camera.toIsometricProps cam of
        Nothing ->
            cam

        Just iso ->
            if Elm3d.Vector2.length input == 0 then
                cam

            else
                cam
                    |> Elm3d.Camera.withOffset
                        (Math.Vector2.add
                            iso.offset
                            input
                        )


size : Camera model msg -> Float
size cam =
    case Elm3d.Camera.toIsometricProps cam of
        Nothing ->
            0

        Just iso ->
            iso.size
