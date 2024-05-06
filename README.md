# @ryan-haskell/elm-3d
> A 3D game engine for Elm

## Local installation

```
elm install ryan-haskell/elm-3d
```

## Quick example

![Rendering 3D Stuff](./example/webgl_obj.gif)

```elm
module Main exposing (main)

import Elm3d.Camera exposing (Camera)
import Elm3d.Node exposing (Node)
import Elm3d.Program exposing (Program)
import Elm3d.Vector3
import Elm3d.Window


main : Program
main =
    Elm3d.Program.new
        { window = Elm3d.Window.fullscreenAspect (16 / 9)
        , nodes =
            [ light
            , buildings
            ]
        , camera = camera
        }

camera : Camera
camera =
    Elm3d.Camera.perspective
        { fov = 60
        , near = 0.01
        , far = 1000
        }
        |> Elm3d.Camera.withPosition (Elm3d.Vector3.new 0 0 4)

light : Node
light =
    Elm3d.Node.light
        { direction = Elm3d.Vector3.new 1 1 1
        }


buildings : Node
buildings =
    Elm3d.Node.group [ tavern, church ]
        |> Elm3d.Node.withOnUpdate rotateEveryFrame


tavern : Node
tavern =
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/building_tavern_blue.obj"
        }
        |> Elm3d.Node.withPosition (Elm3d.Vector3.new 1.0 -0.5 0)


church : Church
church =
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/building_church_blue.obj"
        }
        |> Elm3d.Node.withPosition (Elm3d.Vector3.new -1.0 -0.5 0)


rotateEveryFrame : Elm3d.Node.Context -> Node -> Node
rotateEveryFrame { dt } node =
    node
        |> Elm3d.Node.rotateY dt

```