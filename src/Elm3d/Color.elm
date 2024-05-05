module Elm3d.Color exposing (Color, rgb)

import Elm3d.Vector4 exposing (Vector4)


type alias Color =
    Vector4


rgb : Float -> Float -> Float -> Color
rgb r g b =
    Elm3d.Vector4.new r g b 1.0
