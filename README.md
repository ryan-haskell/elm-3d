# @ryan-haskell/elm-3d
> A 3D game engine for Elm

## Local installation

```
elm install ryan-haskell/elm-3d
```

## Quick example


<img align="center" src="./example/webgl_obj_mtl.gif" />


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
        , camera =
            Elm3d.Camera.perspective
                { fov = 60
                , near = 0.01
                , far = 1000
                }
                |> Elm3d.Camera.withPosition (Elm3d.Vector3.new 0 0 4)
        , nodes = [ buildings ]
        }


buildings : Node
buildings =
    Elm3d.Node.group
        [ tavern
        , church
        ]
        |> Elm3d.Node.withPositionY -0.5
        |> Elm3d.Node.withOnUpdate rotateEveryFrame


tavern : Node
tavern =
    Elm3d.Node.obj
        { url = "http://localhost:3000/medieval_hexagon/building_tavern_blue.obj"
        }
        |> Elm3d.Node.withPositionX 0.75


church : Node
church =
    Elm3d.Node.obj
        { url = "http://localhost:3000/medieval_hexagon/building_church_blue.obj"
        }
        |> Elm3d.Node.withPositionX -0.75


rotateEveryFrame : Elm3d.Node.Context -> Node -> Node
rotateEveryFrame { dt } node =
    node
        |> Elm3d.Node.rotateY (dt * 0.5)

```