module Elm3d.Texture exposing
    ( Texture(..)
    , color
    , rgb
    , rgba
    )

import Elm3d.Color


type Texture
    = Color Elm3d.Color.Color



-- COLORS


color : Elm3d.Color.Color -> Texture
color color_ =
    Color color_


rgb : Int -> Int -> Int -> Texture
rgb r g b =
    Color (Elm3d.Color.rgb r g b)


rgba : Int -> Int -> Int -> Float -> Texture
rgba r g b a =
    Color (Elm3d.Color.rgba r g b a)
