module Elm3d.Vector4 exposing
    ( Vector4
    , new, fromRecord, fromFloat
    , zero, one
    , positiveX, positiveY, positiveZ, positiveW
    , negativeX, negativeY, negativeZ, negativeW
    , add, direction, multiply
    , scale, dot
    , normalize
    , clamp
    , x, y, z, w
    , toRecord
    , length
    )

{-| This module allows you to create, transform, and read data from 4D vectors.


# **Creating vectors**

@docs Vector4
@docs new, fromRecord, fromFloat


## **Common 4D vectors**

@docs zero, one
@docs positiveX, positiveY, positiveZ, positiveW
@docs negativeX, negativeY, negativeZ, negativeW


# **Transforming vectors**

@docs add, direction, multiply
@docs scale, dot
@docs normalize
@docs clamp


# **Getting data from vectors**

@docs x, y, z, w
@docs toRecord
@docs length

-}

import Math.Vector4



-- CREATE


{-| Represents a vector in 4D space
-}
type alias Vector4 =
    Math.Vector4.Vec4


{-| Create a vector from two floats

    position : Vector4
    position =
        new 3 4 5 6

-}
new : Float -> Float -> Float -> Float -> Vector4
new x_ y_ z_ w_ =
    Math.Vector4.vec4 x_ y_ z_ w_


{-| Create a vector from a record

    position : Vector4
    position =
        fromRecord { x = 3, y = 4, z = 5, w = 6 }

-}
fromRecord : { x : Float, y : Float, z : Float, w : Float } -> Vector4
fromRecord record =
    Math.Vector4.fromRecord record


{-| Create a vector from a single float

    fromFloat 10 == new 10 10 10 10

    fromFloat 25
        == fromRecord
            { x = 25
            , y = 25
            , z = 25
            , w = 25
            }

-}
fromFloat : Float -> Vector4
fromFloat num =
    Math.Vector4.vec4 num num num num


{-| Create the vector (0, 0, 0, 0)

    zero == fromRecord { x = 0, y = 0, z = 0, w = 0 }

    zero == new 0 0 0 0

    zero == fromFloat 0

-}
zero : Vector4
zero =
    Math.Vector4.vec4 0 0 0 0


{-| Create the vector (1, 1, 1, 1)

    one == fromRecord { x = 1, y = 1, z = 1, w = 1 }

    one == new 1 1 1 1

    one == fromFloat 1

-}
one : Vector4
one =
    Math.Vector4.vec4 1 1 1 1


{-| Create the unit vector (1, 0, 0, 0)

    positiveX == fromRecord { x = 1, y = 0, z = 0, w = 0 }

    positiveX == new 1 0 0 0

-}
positiveX : Vector4
positiveX =
    Math.Vector4.vec4 1 0 0 0


{-| Create the unit vector (0, 1, 0, 0)

    positiveY == fromRecord { x = 0, y = 1, z = 0, w = 0 }

    positiveY == new 0 1 0 0

-}
positiveY : Vector4
positiveY =
    Math.Vector4.vec4 0 1 0 0


{-| Create the unit vector (0, 0, 1, 0)

    positiveZ == fromRecord { x = 0, y = 0, z = 1, w = 0 }

    positiveZ == new 0 0 1 0

-}
positiveZ : Vector4
positiveZ =
    Math.Vector4.vec4 0 0 1 0


{-| Create the unit vector (0, 0, 0, 1)

    positiveW == fromRecord { x = 0, y = 0, z = 0, w = 1 }

    positiveW == new 0 0 0 1

-}
positiveW : Vector4
positiveW =
    Math.Vector4.vec4 0 0 0 1


{-| Create the unit vector (-1, 0, 0, 0)

    negativeX == fromRecord { x = -1, y = 0, z = 0, w = 0 }

    negativeX == new -1 0 0 0

    negativeX == scale -1 positiveX

-}
negativeX : Vector4
negativeX =
    Math.Vector4.vec4 -1 0 0 0


{-| Create the unit vector (0, -1, 0, 0)

    negativeY == fromRecord { x = 0, y = -1, z = 0, w = 0 }

    negativeY == new 0 -1 0 0

    negativeY == scale -1 positiveY

-}
negativeY : Vector4
negativeY =
    Math.Vector4.vec4 0 -1 0 0


{-| Create the unit vector (0, 0, -1, 0)

    negativeZ == fromRecord { x = 0, y = 0, z = -1, w = 0 }

    negativeZ == new 0 0 -1 0

    negativeZ == scale -1 positiveZ

-}
negativeZ : Vector4
negativeZ =
    Math.Vector4.vec4 0 0 -1 0


{-| Create the unit vector (0, 0, 0, -1)

    negativeW == fromRecord { x = 0, y = 0, z = 0, w = -1 }

    negativeW == new 0 0 0 -1

    negativeW == scale -1 positiveW

-}
negativeW : Vector4
negativeW =
    Math.Vector4.vec4 0 0 0 -1



-- TRANSFORM


{-| A unit vector with the same direction as the given vector.

    normalize (new 1 0 0 0) == new 1 0 0 0

    normalize (new 9 0 0 0) == new 1 0 0 0

    normalize (new 1 1 1 1) == new 0.5 0.5 0.5 0.5

For convenience, normalizing a vector of length zero, returns the input vector:

    normalize (new 0 0 0 0) == new 0 0 0 0

-}
normalize : Vector4 -> Vector4
normalize vec4 =
    if vec4 == zero then
        zero

    else
        Math.Vector4.normalize vec4


{-| Clamp a 4D vector between a min and max vector

    original = new 10 -50 2 0

    clamped =
        original
            |> clamp (new 0 0 0 0) (new 5 5 5 5)

    clamped == new 5 0 2 0

-}
clamp : Vector4 -> Vector4 -> Vector4 -> Vector4
clamp minV maxV valueV =
    let
        min : { x : Float, y : Float, z : Float, w : Float }
        min =
            toRecord minV

        max : { x : Float, y : Float, z : Float, w : Float }
        max =
            toRecord maxV

        val : { x : Float, y : Float, z : Float, w : Float }
        val =
            toRecord valueV
    in
    fromRecord
        { x = val.x |> Basics.clamp min.x max.x
        , y = val.y |> Basics.clamp min.y max.y
        , z = val.z |> Basics.clamp min.z max.z
        , w = val.w |> Basics.clamp min.w max.w
        }


{-| Multiply one vector by another. The resulting vector has the value `(x1 * x3, y1 * y3)`

    multiply (new 3 4 1 0) (new 1 1 1 0) == new 3 4 1 0

    multiply (new 1 1 1 0) (new 3 4 1 0) == new 3 4 1 0

    multiply (new 3 4 5 0) (new 1 0 0 0) == new 3 0 0 0

    multiply (new 3 4 5 0) (new 2 0 0 0) == new 6 0 0 0

    multiply zero one == zero

-}
multiply : Vector4 -> Vector4 -> Vector4
multiply a b =
    new
        (x a * x b)
        (y a * y b)
        (z a * z b)
        (w a * w b)


{-| Multiply the vector by a scalar value.

    scale 3 (new 1 1 1 1) == new 3 3 3 3

    scale 5 (new 1 1 1 1) == new 5 5 5 5

    scale 5 (new 3 0 0 0) == new 10 0 0 0

    scale 5 (new 3 4 5 6) == multiply (new 5 5 5 5) (new 3 4 5 6)

    scale 0 one == zero

-}
scale : Float -> Vector4 -> Vector4
scale constant vec4 =
    Math.Vector4.scale constant vec4


{-| Get the dot product between two vectors

    dot positiveX positiveX == 1

    dot positiveX negativeX == -1

    dot positiveX positiveY == 0

    dot positiveX (new 5 0 0 0) == 5

-}
dot : Vector4 -> Vector4 -> Float
dot a b =
    Math.Vector4.dot a b


{-| Add two vectors together

    add (new 1 0 0 0) (new 2 1 4 5) == new 3 1 4 5

    add zero one == one

-}
add : Vector4 -> Vector4 -> Vector4
add a b =
    Math.Vector4.add a b


{-| Get the direction from one vector to another.

    playerPosition : Vector4
    playerPosition =
        new 3 4 5 0

    enemyPosition : Vector4
    enemyPosition =
        new 1 0 8 0

    playerToEnemy : Vector4
    playerToEnemy =
        direction
            { from = playerPosition
            , to = enemyPosition
            }

**Fun fact**: This is the same as _subtracting_ one vector from the other!

-}
direction : { from : Vector4, to : Vector4 } -> Vector4
direction { from, to } =
    Math.Vector4.direction to from



-- READ


{-| Get the x-coordinate for this vector
-}
x : Vector4 -> Float
x vec4 =
    Math.Vector4.getX vec4


{-| Get the y-coordinate for this vector
-}
y : Vector4 -> Float
y vec4 =
    Math.Vector4.getY vec4


{-| Get the z-coordinate for this vector
-}
z : Vector4 -> Float
z vec4 =
    Math.Vector4.getZ vec4


{-| Get the w-coordinate for this vector
-}
w : Vector4 -> Float
w vec4 =
    Math.Vector4.getW vec4


{-| Get the vector as an Elm record
-}
toRecord : Vector4 -> { x : Float, y : Float, z : Float, w : Float }
toRecord vec4 =
    { x = Math.Vector4.getX vec4
    , y = Math.Vector4.getY vec4
    , z = Math.Vector4.getZ vec4
    , w = Math.Vector4.getW vec4
    }


{-| Get the length of the current vector

    length (new 1 0 0 0) == 1

    length (new 3 4 0 0) == 5

-}
length : Vector4 -> Float
length v =
    Math.Vector4.length v
