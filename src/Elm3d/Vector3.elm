module Elm3d.Vector3 exposing
    ( Vector3
    , new, one, zero
    , positiveX, positiveY, positiveZ
    , fromRecord
    )

{-|

@docs Vector3

@docs new, one, zero
@docs positiveX, positiveY, positiveZ
@docs fromRecord

-}

import Math.Vector3


type alias Vector3 =
    Math.Vector3.Vec3


zero : Vector3
zero =
    Math.Vector3.vec3 0 0 0


one : Vector3
one =
    Math.Vector3.vec3 1 1 1


positiveX : Vector3
positiveX =
    Math.Vector3.vec3 1 0 0


positiveY : Vector3
positiveY =
    Math.Vector3.vec3 0 1 0


positiveZ : Vector3
positiveZ =
    Math.Vector3.vec3 0 0 1


new : Float -> Float -> Float -> Vector3
new x y z =
    Math.Vector3.vec3 x y z


fromRecord : { x : Float, y : Float, z : Float } -> Vector3
fromRecord record =
    Math.Vector3.fromRecord record
