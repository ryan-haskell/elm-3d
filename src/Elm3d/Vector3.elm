module Elm3d.Vector3 exposing
    ( Vector3
    , new, fromRecord, fromFloat
    , zero, one
    , positiveX, positiveY, positiveZ
    , negativeX, negativeY, negativeZ
    , add, direction, multiply
    , scale, dot
    , normalize
    , clamp
    , x, y, z
    , toRecord
    , length
    )

{-| This module allows you to create, transform, and read data from 3D vectors.


# **Creating vectors**

@docs Vector3
@docs new, fromRecord, fromFloat


## **Common 3D vectors**

@docs zero, one
@docs positiveX, positiveY, positiveZ
@docs negativeX, negativeY, negativeZ


# **Transforming vectors**

@docs add, direction, multiply
@docs scale, dot
@docs normalize
@docs clamp


# **Getting data from vectors**

@docs x, y, z
@docs toRecord
@docs length

-}

import Math.Vector3



-- CREATE


{-| Represents a vector in 3D space
-}
type alias Vector3 =
    Math.Vector3.Vec3


{-| Create a vector from two floats

    position : Vector3
    position =
        new 3 4 5

-}
new : Float -> Float -> Float -> Vector3
new x_ y_ z_ =
    Math.Vector3.vec3 x_ y_ z_


{-| Create a vector from a record

    position : Vector3
    position =
        fromRecord { x = 3, y = 4, z = 5 }

-}
fromRecord : { x : Float, y : Float, z : Float } -> Vector3
fromRecord record =
    Math.Vector3.fromRecord record


{-| Create a vector from a single float

    fromFloat 10 == new 10 10 10

    fromFloat 25 == fromRecord { x = 25, y = 25, z = 25 }

-}
fromFloat : Float -> Vector3
fromFloat num =
    Math.Vector3.vec3 num num num


{-| Create the vector (0, 0, 0)

    zero == fromRecord { x = 0, y = 0, z = 0 }

    zero == new 0 0 0

    zero == fromFloat 0

-}
zero : Vector3
zero =
    Math.Vector3.vec3 0 0 0


{-| Create the vector (1, 1, 1)

    one == fromRecord { x = 1, y = 1, z = 1 }

    one == new 1 1 1

    one == fromFloat 1

-}
one : Vector3
one =
    Math.Vector3.vec3 1 1 1


{-| Create the unit vector (1, 0, 0)

    positiveX == fromRecord { x = 1, y = 0, z = 0 }

    positiveX == new 1 0 0

-}
positiveX : Vector3
positiveX =
    Math.Vector3.vec3 1 0 0


{-| Create the unit vector (0, 1, 0)

    positiveY == fromRecord { x = 0, y = 1, z = 0 }

    positiveY == new 0 1 0

-}
positiveY : Vector3
positiveY =
    Math.Vector3.vec3 0 1 0


{-| Create the unit vector (0, 0, 1)

    positiveZ == fromRecord { x = 0, y = 0, z = 1 }

    positiveZ == new 0 0 1

-}
positiveZ : Vector3
positiveZ =
    Math.Vector3.vec3 0 0 1


{-| Create the unit vector (-1, 0, 0)

    negativeX == fromRecord { x = -1, y = 0, z = 0 }

    negativeX == new -1 0 0

    negativeX == scale -1 positiveX

-}
negativeX : Vector3
negativeX =
    Math.Vector3.vec3 -1 0 0


{-| Create the unit vector (0, -1, 0)

    negativeY == fromRecord { x = 0, y = -1, z = 0 }

    negativeY == new 0 -1 0

    negativeY == scale -1 positiveY

-}
negativeY : Vector3
negativeY =
    Math.Vector3.vec3 0 -1 0


{-| Create the unit vector (0, 0, -1)

    negativeZ == fromRecord { x = 0, y = 0, z = -1 }

    negativeZ == new 0 0 -1

    negativeZ == scale -1 positiveZ

-}
negativeZ : Vector3
negativeZ =
    Math.Vector3.vec3 0 0 -1



-- TRANSFORM


{-| A unit vector with the same direction as the given vector.

    normalize (new 1 0 0) == new 1 0 0

    normalize (new 9 0 0) == new 1 0 0

    normalize (new 1 1 1)
        == new
            (sqrt (1 / 3))
            (sqrt (1 / 3))
            (sqrt (1 / 3))

For convenience, normalizing a vector of length zero, returns the input vector:

    normalize (new 0 0 0) == new 0 0 0

-}
normalize : Vector3 -> Vector3
normalize vec3 =
    if vec3 == zero then
        zero

    else
        Math.Vector3.normalize vec3


{-| Clamp a 3D vector between a min and max vector

    original = new 10 -50 2

    clamped =
        original
            |> clamp (new 0 0 0) (new 5 5 5)

    clamped == new 5 0 2

-}
clamp : Vector3 -> Vector3 -> Vector3 -> Vector3
clamp minV maxV valueV =
    let
        min : { x : Float, y : Float, z : Float }
        min =
            toRecord minV

        max : { x : Float, y : Float, z : Float }
        max =
            toRecord maxV

        val : { x : Float, y : Float, z : Float }
        val =
            toRecord valueV
    in
    fromRecord
        { x = val.x |> Basics.clamp min.x max.x
        , y = val.y |> Basics.clamp min.y max.y
        , z = val.z |> Basics.clamp min.z max.z
        }


{-| Multiply one vector by another. The resulting vector has the value `(x1 * x3, y1 * y3)`

    multiply (new 3 4 1) (new 1 1 1) == new 3 4 1

    multiply (new 1 1 1) (new 3 4 1) == new 3 4 1

    multiply (new 3 4 5) (new 1 0 0) == new 3 0 0

    multiply (new 3 4 5) (new 2 0 0) == new 6 0 0

    multiply zero one == zero

-}
multiply : Vector3 -> Vector3 -> Vector3
multiply a b =
    new
        (x a * x b)
        (y a * y b)
        (z a * z b)


{-| Multiply the vector by a scalar value.

    scale 3 (new 1 1 1) == new 3 3 3

    scale 5 (new 1 1 1) == new 5 5 5

    scale 5 (new 3 0 0) == new 10 0 0

    scale 5 (new 3 4 5) == multiply (new 5 5 5) (new 3 4 5)

    scale 0 one == zero

-}
scale : Float -> Vector3 -> Vector3
scale constant vec3 =
    Math.Vector3.scale constant vec3


{-| Get the dot product between two vectors

    dot positiveX positiveX == 1

    dot positiveX negativeX == -1

    dot positiveX positiveY == 0

    dot positiveX (new 5 0 0) == 5

-}
dot : Vector3 -> Vector3 -> Float
dot a b =
    Math.Vector3.dot a b


{-| Add two vectors together

    add (new 1 0 0) (new 2 1 4) == new 3 1 4

    add zero one == one

-}
add : Vector3 -> Vector3 -> Vector3
add a b =
    Math.Vector3.add a b


{-| Get the direction from one vector to another.

    playerPosition : Vector3
    playerPosition =
        new 3 4 5

    enemyPosition : Vector3
    enemyPosition =
        new 1 0 8

    playerToEnemy : Vector3
    playerToEnemy =
        direction
            { from = playerPosition
            , to = enemyPosition
            }

**Fun fact**: This is the same as _subtracting_ one vector from the other!

-}
direction : { from : Vector3, to : Vector3 } -> Vector3
direction { from, to } =
    Math.Vector3.direction to from



-- READ


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


{-| Get the vector as an Elm record
-}
toRecord : Vector3 -> { x : Float, y : Float, z : Float }
toRecord vec3 =
    { x = Math.Vector3.getX vec3
    , y = Math.Vector3.getY vec3
    , z = Math.Vector3.getZ vec3
    }


{-| Get the length of the current vector

    length (new 1 0 0) == 1

    length (new 3 4 0) == 5

-}
length : Vector3 -> Float
length v =
    Math.Vector3.length v
