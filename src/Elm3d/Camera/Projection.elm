module Elm3d.Camera.Projection exposing (..)


type Projection
    = Orthographic
        { size : Float
        , near : Float
        , far : Float
        }
    | Perspective
        { fov : Float
        , near : Float
        , far : Float
        }
