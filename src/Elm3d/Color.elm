module Elm3d.Color exposing
    ( Color
    , rgb, rgba
    , white, black, transparent
    , red, green, blue
    , magenta, cyan, yellow
    , toHtmlColor
    )

{-|

@docs Color

@docs rgb, rgba
@docs white, black, transparent
@docs red, green, blue
@docs magenta, cyan, yellow

@docs toHtmlColor

-}

import Elm3d.Vector4 exposing (Vector4)


type alias Color =
    Vector4


rgb : Int -> Int -> Int -> Color
rgb r g b =
    rgba r g b 1.0


rgba : Int -> Int -> Int -> Float -> Color
rgba r g b a =
    Elm3d.Vector4.new
        (toFloat r / 255)
        (toFloat g / 255)
        (toFloat b / 255)
        a



-- GRAYSCALE


transparent : Color
transparent =
    Elm3d.Vector4.zero


white : Color
white =
    rgb 255 255 255


black : Color
black =
    rgb 0 0 0



-- RGB


red : Color
red =
    rgb 255 0 0


green : Color
green =
    rgb 0 255 0


blue : Color
blue =
    rgb 0 0 255



-- CMY


magenta : Color
magenta =
    rgb 255 0 255


cyan : Color
cyan =
    rgb 0 255 255


yellow : Color
yellow =
    rgb 255 255 0



-- INTERNALS


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
