module Elm3d.Float exposing (lerp)


lerp :
    { from : Float
    , to : Float
    , step : Float
    }
    -> Float
lerp { from, to, step } =
    (to - from) * clamp 0 1 step + from
