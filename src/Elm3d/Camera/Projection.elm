module Elm3d.Camera.Projection exposing (..)

import Elm3d.Vector2 exposing (Vector2)


type Projection
    = Orthographic OrthographicProps
    | Perspective PerspectiveProps
    | Isometric IsometricProps


type alias OrthographicProps =
    { size : Float
    , range : ( Float, Float )
    }


type alias PerspectiveProps =
    { fov : Float
    , range : ( Float, Float )
    }


type alias IsometricProps =
    { size : Float
    , range : ( Float, Float )
    , rotation : Float
    , angle : Float
    , distance : Float
    , offset : Vector2
    }
