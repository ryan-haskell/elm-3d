module Elm3d.Color exposing
    ( Color
    , rgb, rgba
    , white, black, transparent
    , red, green, blue
    , magenta, cyan, yellow
    , toHtmlColor
    )

{-| This module allows you to define RGB colors as `Vector4` values. This makes them
compatible with WebGL and they can support alpha transparency.


# **Creating colors**

@docs Color
@docs rgb, rgba


## **Common colors**

@docs white, black, transparent
@docs red, green, blue
@docs magenta, cyan, yellow


## **Rendering colors**

@docs toHtmlColor

-}

import Elm3d.Vector4 exposing (Vector4)


{-| Represents a standard color value.
-}
type alias Color =
    Vector4


{-| Create a color from RGB integers (0-255)

    rgb 255 255 255 == white

    rgb 0 0 0 == black

    rgb 255 0 0 == red

    rgb 0 255 0 == green

    rgb 0 0 255 == blue

-}
rgb : Int -> Int -> Int -> Color
rgb r g b =
    rgba r g b 1.0


{-| Create a color that supports transparency

    rgba 255 255 255 1 == white

    rgba 0 0 0 1 == black

    rgba 0 0 0 0 == transparent

-}
rgba : Int -> Int -> Int -> Float -> Color
rgba r g b a =
    Elm3d.Vector4.new
        (toFloat (r |> clamp 0 255) / 255)
        (toFloat (g |> clamp 0 255) / 255)
        (toFloat (b |> clamp 0 255) / 255)
        (a |> clamp 0 1)



-- GRAYSCALE


{-| Completely transparent

    transparent == rgba 0 0 0 0

-}
transparent : Color
transparent =
    Elm3d.Vector4.zero


{-|

    white == rgb 255 255 255

-}
white : Color
white =
    rgb 255 255 255


{-|

    black == rgb 0 0 0

-}
black : Color
black =
    rgb 0 0 0



-- RGB


{-| The primary red color.

    red == rgb 255 0 0

-}
red : Color
red =
    rgb 255 0 0


{-| The primary green color.

    green == rgb 0 255 0

-}
green : Color
green =
    rgb 0 255 0


{-| The primary blue color.

    blue == rgb 0 0 255

-}
blue : Color
blue =
    rgb 0 0 255



-- CMY


{-| The secondary magenta color

    magenta == rgb 255 0 255

-}
magenta : Color
magenta =
    rgb 255 0 255


{-| The secondary cyan color

    cyan == rgb 0 255 255

-}
cyan : Color
cyan =
    rgb 0 255 255


{-| The secondary yellow color

    yellow == rgb 255 255 0

-}
yellow : Color
yellow =
    rgb 255 255 0



-- INTERNALS


{-| Convert a color to a string that can be used in a CSS style tag

    toHtmlColor red == "rgba(255, 0, 0, 1.0)"

    toHtmlColor magenta == "rgba(255, 0, 255, 1.0)"

    toHtmlColor black == "rgba(0, 0, 0, 1.0)"

    toHtmlColor transparent == "rgba(0, 0, 0, 0.0)"

-}
toHtmlColor : Color -> String
toHtmlColor vec4 =
    let
        color =
            Elm3d.Vector4.toRecord vec4
    in
    "rgba("
        ++ toString255 color.x
        ++ ", "
        ++ toString255 color.y
        ++ ", "
        ++ toString255 color.z
        ++ ", "
        ++ String.fromFloat color.w
        ++ ")"


toString255 : Float -> String
toString255 percent =
    floor (255 * percent)
        |> String.fromInt
