module Elm3d.Texture exposing
    ( Texture(..)
    , color, rgb, rgba
    )

{-|

@docs Texture

@docs color, rgb, rgba

-}

import Elm3d.Color


{-| Represents a Texture that can be used with a mesh.
-}
type Texture
    = Color Elm3d.Color.Color



-- COLORS


{-| Create a texture from an existing color value

    import Elm3d.Color

    blueTexture : Texture
    blueTexture =
        color Elm3d.Color.blue

-}
color : Elm3d.Color.Color -> Texture
color color_ =
    Color color_


{-| Create a texture from RGB values

    redTexture : Texture
    redTexture =
        rgb 255 0 0

-}
rgb : Int -> Int -> Int -> Texture
rgb r g b =
    Color (Elm3d.Color.rgb r g b)


{-| Create a texture from an RGBA value

    halfGreenTexture : Texture
    halfGreenTexture =
        rgb 0 255 0 0.5

-}
rgba : Int -> Int -> Int -> Float -> Texture
rgba r g b a =
    Color (Elm3d.Color.rgba r g b a)
