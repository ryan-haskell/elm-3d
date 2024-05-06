module Elm3d.Texture exposing
    ( Texture(..)
    , color
    , rgb
    )

import Elm3d.Color


type Texture
    = Color Elm3d.Color.Color


color : Elm3d.Color.Color -> Texture
color color_ =
    Color color_


rgb : Float -> Float -> Float -> Texture
rgb r g b =
    Color (Elm3d.Color.rgb r g b)
