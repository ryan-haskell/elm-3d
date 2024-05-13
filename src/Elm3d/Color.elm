module Elm3d.Color exposing
    ( Color
    , rgb
    , rgba
    , toHtmlColor
    , transparent
    )

import Elm3d.Vector4 exposing (Vector4)
import Math.Vector4


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


transparent : Color
transparent =
    Elm3d.Vector4.zero


toHtmlColor : Color -> String
toHtmlColor vec4 =
    let
        color =
            Math.Vector4.toRecord vec4
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
