module Elm3d.Camera.Projection exposing (..)

import Elm3d.Vector2 exposing (Vector2)


type Projection
    = Orthographic OrthographicProps
    | Perspective PerspectiveProps
    | Isometric IsometricProps


type alias OrthographicProps =
    { size : Float
    , near : Float
    , far : Float
    }


type alias PerspectiveProps =
    { fov : Float
    , near : Float
    , far : Float
    }


type alias IsometricProps =
    { size : Float
    , near : Float
    , far : Float
    , rotation : Float
    , angle : Float
    , distance : Float
    , offset : Vector2
    }
