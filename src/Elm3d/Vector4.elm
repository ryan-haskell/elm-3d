module Elm3d.Vector4 exposing
    ( Vector4
    , new, one, zero
    , fromRecord
    , toRecord
    )

{-|

@docs Vector4

@docs new, one, zero
@docs fromRecord

@docs toRecord

-}

import Math.Vector4


type alias Vector4 =
    Math.Vector4.Vec4


zero : Vector4
zero =
    Math.Vector4.vec4 0 0 0 0


one : Vector4
one =
    Math.Vector4.vec4 1 1 1 1


new : Float -> Float -> Float -> Float -> Vector4
new x y z w =
    Math.Vector4.vec4 x y z w


fromRecord : { x : Float, y : Float, z : Float, w : Float } -> Vector4
fromRecord record =
    Math.Vector4.fromRecord record


toRecord : Vector4 -> { x : Float, y : Float, z : Float, w : Float }
toRecord vec4 =
    Math.Vector4.toRecord vec4
