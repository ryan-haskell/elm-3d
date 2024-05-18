module Elm3d.Context exposing
    ( Context
    , isKeyPressed
    , isLeftClickPressed
    , toInputAxis
    , toInputVector
    )

import Elm3d.Input
import Elm3d.Input.Key exposing (Key)
import Elm3d.Vector2 exposing (Vector2)


type alias Context =
    { dt : Float
    , time : Float
    , input : Elm3d.Input.Model
    }


isKeyPressed : Key -> Context -> Bool
isKeyPressed key ctx =
    Elm3d.Input.isKeyPressed ctx.input key


isLeftClickPressed : Context -> Bool
isLeftClickPressed ctx =
    Elm3d.Input.isLeftClickPressed ctx.input


{-| Commonly used when working with directional user input
-}
toInputVector : Context -> { x : ( Key, Key ), y : ( Key, Key ) } -> Vector2
toInputVector ctx keys =
    let
        x =
            toInputAxis ctx keys.x

        y =
            toInputAxis ctx keys.y
    in
    if x == 0 && y == 0 then
        Elm3d.Vector2.zero

    else
        Elm3d.Vector2.new x y
            |> Elm3d.Vector2.normalize


{-| Commonly used when working with directional user input
-}
toInputAxis : Context -> ( Key, Key ) -> Float
toInputAxis ctx ( left, right ) =
    if isKeyPressed right ctx then
        if isKeyPressed left ctx then
            0

        else
            1

    else if isKeyPressed left ctx then
        -1

    else
        0
