module Elm3d.Context exposing
    ( Context
    , isKeyPressed
    , toInputAxis
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


{-| Commonly used when working with directional user input
-}
toInputAxis : Context -> { x : ( Key, Key ), y : ( Key, Key ) } -> Vector2
toInputAxis ctx keys =
    let
        ( keyLeft, keyRight ) =
            keys.x

        ( keyDown, keyUp ) =
            keys.y
    in
    Elm3d.Vector2.new
        (if isKeyPressed keyRight ctx then
            1

         else if isKeyPressed keyLeft ctx then
            -1

         else
            0
        )
        (if isKeyPressed keyUp ctx then
            1

         else if isKeyPressed keyDown ctx then
            -1

         else
            0
        )
