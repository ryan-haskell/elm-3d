module Elm3d.Vector2 exposing
    ( Vector2
    , new, fromRecord, fromFloat
    , zero, one
    , positiveX, positiveY
    , negativeX, negativeY
    , add, direction, multiply
    , scale
    , normalize, rotate
    , clamp
    , x, y, toRecord
    , length
    )

{-| This module allows you to create, transform, and read data from 2D vectors.


# **Creating vectors**

@docs Vector2
@docs new, fromRecord, fromFloat


## **Common 2D vectors**

@docs zero, one
@docs positiveX, positiveY
@docs negativeX, negativeY


# **Transforming vectors**

@docs add, direction, multiply
@docs scale
@docs normalize, rotate
@docs clamp


# **Getting data from vectors**

@docs x, y, toRecord
@docs length

-}

import Math.Vector2



-- CREATE


{-| Represents a vector in 2D space
-}
type alias Vector2 =
    Math.Vector2.Vec2


{-| Create a vector from two floats

    position : Vector2
    position =
        new 3 4

-}
new : Float -> Float -> Vector2
new x_ y_ =
    Math.Vector2.vec2 x_ y_


{-| Create a vector from a record

    position : Vector2
    position =
        fromRecord { x = 3, y = 4 }

-}
fromRecord : { x : Float, y : Float } -> Vector2
fromRecord record =
    Math.Vector2.fromRecord record


{-| Create a vector from a single float

    fromFloat 10 == new 10 10

    fromFloat 20 == fromRecord { x = 20, y = 20 }

-}
fromFloat : Float -> Vector2
fromFloat num =
    Math.Vector2.vec2 num num


{-| Create the vector (0, 0)

    zero == fromRecord { x = 0, y = 0 }

    zero == new 0 0

    zero == fromFloat 0

-}
zero : Vector2
zero =
    Math.Vector2.vec2 0 0


{-| Create the vector (1, 1)

    one == fromRecord { x = 1, y = 1 }

    one == new 1 1

    one == fromFloat 1

-}
one : Vector2
one =
    Math.Vector2.vec2 1 1


{-| Create the unit vector (1, 0)

    positiveX == fromRecord { x = 1, y = 0 }

    positiveX == new 1 0

-}
positiveX : Vector2
positiveX =
    Math.Vector2.vec2 1 0


{-| Create the unit vector (0, 1)

    positiveY == fromRecord { x = 0, y = 1 }

    positiveY == new 0 1

-}
positiveY : Vector2
positiveY =
    Math.Vector2.vec2 0 1


{-| Create the unit vector (-1, 0)

    negativeX == fromRecord { x = -1, y = 0 }

    negativeX == new -1 0

    negativeX == scale -1 positiveX

-}
negativeX : Vector2
negativeX =
    Math.Vector2.vec2 -1 0


{-| Create the unit vector (0, -1)

    negativeY == fromRecord { x = 0, y = -1 }

    negativeY == new 0 -1

    negativeY == scale -1 positiveY

-}
negativeY : Vector2
negativeY =
    Math.Vector2.vec2 0 -1



-- TRANSFORM


{-| A unit vector with the same direction as the given vector.

    normalize (new 1 0) == new 1 0

    normalize (new 2 0) == new 1 0

    normalize (new 1 1) == new (sqrt 0.5) (sqrt 0.5)

    normalize (new 10 10) == new (sqrt 0.5) (sqrt 0.5)

For convenience, normalizing a vector of length zero, returns the input vector:

    normalize (new 0 0) == new 0 0

-}
normalize : Vector2 -> Vector2
normalize vec2 =
    if vec2 == zero then
        zero

    else
        Math.Vector2.normalize vec2


{-| Clamp a 2D vector between a min and max vector

    original = new 10 -50

    clamped =
        original
            |> clamp (new 0 0) (new 5 5)

    clamped == new 5 0

-}
clamp : Vector2 -> Vector2 -> Vector2 -> Vector2
clamp minV maxV valueV =
    let
        min : { x : Float, y : Float }
        min =
            toRecord minV

        max : { x : Float, y : Float }
        max =
            toRecord maxV

        val : { x : Float, y : Float }
        val =
            toRecord valueV
    in
    fromRecord
        { x = val.x |> Basics.clamp min.x max.x
        , y = val.y |> Basics.clamp min.y max.y
        }


{-| Multiply one vector by another. The resulting vector has the value `(x1 * x2, y1 * y2)`

    multiply (new 3 4) (new 1 1) == new 3 4

    multiply (new 1 1) (new 3 4) == new 3 4

    multiply (new 3 4) (new 1 0) == new 3 0

    multiply (new 3 4) (new 2 0) == new 6 0

    multiply zero one == zero

-}
multiply : Vector2 -> Vector2 -> Vector2
multiply a b =
    new
        (x a * x b)
        (y a * y b)


{-| Multiply the vector by a scalar value.

    scale 2 (new 1 1) == new 2 2

    scale 5 (new 1 1) == new 5 5

    scale 5 (new 2 0) == new 10 0

    scale 5 (new 3 4) == multiply (new 5 5) (new 3 4)

    scale 0 one == zero

-}
scale : Float -> Vector2 -> Vector2
scale constant vec2 =
    Math.Vector2.scale constant vec2


{-| Rotate a vector by the given angle

    rotate pi positiveX == negativeX

    rotate (pi / 2) positiveX == positiveY

-}
rotate : Float -> Vector2 -> Vector2
rotate angle vec2 =
    let
        vector =
            toRecord vec2

        cosAngle =
            cos angle

        sinAngle =
            sin angle

        x2 =
            vector.x * cosAngle - vector.y * sinAngle

        y2 =
            vector.x * sinAngle + vector.y * cosAngle
    in
    fromRecord { x = x2, y = y2 }


{-| Add two vectors together

    add (new 1 0) (new 2 1) == new 3 1

    add zero one == one

-}
add : Vector2 -> Vector2 -> Vector2
add a b =
    Math.Vector2.add a b


{-| Get the direction from one vector to another.

    playerPosition : Vector2
    playerPosition =
        new 4 3

    enemyPosition : Vector2
    enemyPosition =
        new 2 0

    playerToEnemy : Vector2
    playerToEnemy =
        direction
            { from = playerPosition
            , to = enemyPosition
            }

**Fun fact**: This is the same as _subtracting_ one vector from the other!

-}
direction : { from : Vector2, to : Vector2 } -> Vector2
direction { from, to } =
    Math.Vector2.direction to from



-- READ


{-| Get the x-coordinate for this vector
-}
x : Vector2 -> Float
x vec2 =
    Math.Vector2.getX vec2


{-| Get the y-coordinate for this vector
-}
y : Vector2 -> Float
y vec2 =
    Math.Vector2.getY vec2


{-| Get the vector as an Elm record
-}
toRecord : Vector2 -> { x : Float, y : Float }
toRecord vec2 =
    { x = Math.Vector2.getX vec2
    , y = Math.Vector2.getY vec2
    }


{-| Get the length of the current vector

    length (new 1 0) == 1

    length (new 3 4) == 5

-}
length : Vector2 -> Float
length v =
    Math.Vector2.length v
