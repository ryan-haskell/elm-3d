module Elm3d.Vector3 exposing
    ( Vector3
    , new, one, zero
    , positiveX, positiveY, positiveZ
    , fromRecord
    , x, y, z
    )

{-|

@docs Vector3

@docs new, one, zero
@docs positiveX, positiveY, positiveZ
@docs fromRecord

@docs x, y, z

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
new x_ y_ z_ =
    Math.Vector3.vec3 x_ y_ z_


fromRecord : { x : Float, y : Float, z : Float } -> Vector3
fromRecord record =
    Math.Vector3.fromRecord record


x : Vector3 -> Float
x vec3 =
    Math.Vector3.getX vec3


y : Vector3 -> Float
y vec3 =
    Math.Vector3.getY vec3


z : Vector3 -> Float
z vec3 =
    Math.Vector3.getZ vec3
