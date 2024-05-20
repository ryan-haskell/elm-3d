module Elm3d.Internals.Program exposing (..)

import Browser.Dom
import Elm3d.Component


type alias Program flags model msg =
    Platform.Program flags (Model model) (Msg msg)


type Model model
    = Model
        { elm3d : Elm3d.Component.Model
        , user : model
        , window : ( Int, Int )
        }


type Msg msg
    = Elm3d Elm3d.Component.Msg
    | User msg
    | Resize Int Int
    | Viewport Browser.Dom.Viewport
