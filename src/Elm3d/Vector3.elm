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


{-| Represents a vector in 3D space
-}
type alias Vector3 =
    Math.Vector3.Vec3


{-| Create the vector (0, 0, 0)
-}
zero : Vector3
zero =
    Math.Vector3.vec3 0 0 0


{-| Create the vector (1, 1, 1)
-}
one : Vector3
one =
    Math.Vector3.vec3 1 1 1


{-| Create the vector (1, 0, 0)
-}
positiveX : Vector3
positiveX =
    Math.Vector3.vec3 1 0 0


{-| Create the vector (0, 1, 0)
-}
positiveY : Vector3
positiveY =
    Math.Vector3.vec3 0 1 0


{-| Create the vector (0, 0, 1)
-}
positiveZ : Vector3
positiveZ =
    Math.Vector3.vec3 0 0 1


{-| Create a vector from three floats
-}
new : Float -> Float -> Float -> Vector3
new x_ y_ z_ =
    Math.Vector3.vec3 x_ y_ z_


{-| Create a vector from a record
-}
fromRecord : { x : Float, y : Float, z : Float } -> Vector3
fromRecord record =
    Math.Vector3.fromRecord record


{-| Get the x-coordinate for this vector
-}
x : Vector3 -> Float
x vec3 =
    Math.Vector3.getX vec3


{-| Get the y-coordinate for this vector
-}
y : Vector3 -> Float
y vec3 =
    Math.Vector3.getY vec3


{-| Get the z-coordinate for this vector
-}
z : Vector3 -> Float
z vec3 =
    Math.Vector3.getZ vec3
