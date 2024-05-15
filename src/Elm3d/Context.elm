module Elm3d.Context exposing
    ( Context
    , isKeyPressed
    , isLeftClickPressed
    , toInputAxis
    )

import Elm3d.Input
import Elm3d.Input.Key exposing (Key)
import Elm3d.Vector2 exposing (Vector2)


type alias Context model =
    { dt : Float
    , time : Float
    , input : Elm3d.Input.Model
    , model : model
    }


isKeyPressed : Key -> Context model -> Bool
isKeyPressed key ctx =
    Elm3d.Input.isKeyPressed ctx.input key


isLeftClickPressed : Context model -> Bool
isLeftClickPressed ctx =
    Elm3d.Input.isLeftClickPressed ctx.input


{-| Commonly used when working with directional user input
-}
toInputAxis : Context model -> { x : ( Key, Key ), y : ( Key, Key ) } -> Vector2
toInputAxis ctx keys =
    let
        ( keyLeft, keyRight ) =
            keys.x

        ( keyDown, keyUp ) =
            keys.y

        x =
            if isKeyPressed keyRight ctx then
                1

            else if isKeyPressed keyLeft ctx then
                -1

            else
                0

        y =
            if isKeyPressed keyUp ctx then
                1

            else if isKeyPressed keyDown ctx then
                -1

            else
                0
    in
    if x == 0 && y == 0 then
        Elm3d.Vector2.zero

    else
        Elm3d.Vector2.new x y
            |> Elm3d.Vector2.normalize
