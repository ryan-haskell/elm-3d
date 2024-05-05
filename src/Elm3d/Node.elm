module Elm3d.Node exposing
    ( Node
    , cube
    , withPosition
    , withTextureColor
    )

import Elm3d.Color exposing (Color)
import Elm3d.Internal.Node
import Elm3d.Vector3 exposing (Vector3)


type alias Node =
    Elm3d.Internal.Node.Node


cube : { size : Float } -> Node
cube props =
    Elm3d.Internal.Node.cube props


withPosition : Vector3 -> Node -> Node
withPosition props =
    Elm3d.Internal.Node.withPosition props


withTextureColor : Color -> Node -> Node
withTextureColor props =
    Elm3d.Internal.Node.withTextureColor props
