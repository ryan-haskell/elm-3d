module Elm3d.Vector2 exposing
    ( Vector2
    , new, one, zero
    , positiveX, positiveY
    , fromRecord
    , x, y, toRecord
    )

{-|

@docs Vector2

@docs new, one, zero
@docs positiveX, positiveY
@docs fromRecord

@docs x, y, toRecord

-}

import Math.Vector2


{-| Represents a vector in 2D space
-}
type alias Vector2 =
    Math.Vector2.Vec2


{-| Create the vector (0, 0)
-}
zero : Vector2
zero =
    Math.Vector2.vec2 0 0


{-| Create the vector (1, 1)
-}
one : Vector2
one =
    Math.Vector2.vec2 1 1


{-| Create the vector (1, 0)
-}
positiveX : Vector2
positiveX =
    Math.Vector2.vec2 1 0


{-| Create the vector (0, 1)
-}
positiveY : Vector2
positiveY =
    Math.Vector2.vec2 0 1


{-| Create a vector from two floats
-}
new : Float -> Float -> Vector2
new x_ y_ =
    Math.Vector2.vec2 x_ y_


{-| Create a vector from a record
-}
fromRecord : { x : Float, y : Float } -> Vector2
fromRecord record =
    Math.Vector2.fromRecord record


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
