# @ryan-haskell/elm-3d
> A 3D game engine for Elm

## Local installation

```
elm install ryan-haskell/elm-3d
```

## Quick example

<p align="center">
  <img width="800" height="auto" src="./example/webgl_obj_mtl.gif" />
</p>


```elm
module Main exposing (main)

import Elm3d.Camera exposing (Camera)
import Elm3d.Context exposing (Context)
import Elm3d.Node exposing (Node)
import Elm3d.Program exposing (Program, View)
import Elm3d.Viewport


main : Program () Model Msg
main =
    Elm3d.Program.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { angle : Float
    }


init : Model
init =
    { angle = 0
    }



-- UPDATE


type Msg
    = Spin Context Node


update : Msg -> Model -> Model
update msg model =
    case msg of
        Spin ctx node ->
            { model | angle = model.angle + ctx.dt }



-- VIEW


view : Model -> View Msg
view model =
    { viewport = Elm3d.Viewport.fullscreen
    , background = Elm3d.Color.white
    , camera =
        Elm3d.Camera.perspective
            { fov = 60
            , near = 1
            , far = 100
            }
            |> Elm3d.Camera.withPositionZ 5
    , nodes =
        [ buildings model
        ]
    }


buildings : Model -> Node Msg
buildings model =
    Elm3d.Node.group
        [ tavern
        , church
        ]
        |> Elm3d.Node.withOnUpdate Spin
        |> Elm3d.Node.withRotationY model.angle


tavern : Node Msg
tavern =
    Elm3d.Node.obj
        { url = "/assets/buildings/tavern.obj"
        }
        |> Elm3d.Node.withPositionX 1


church : Node Msg
church =
    Elm3d.Node.obj
        { url = "/assets/buildings/church.obj"
        }
        |> Elm3d.Node.withPositionX -1
```
