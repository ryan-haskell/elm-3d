# @ryan-haskell/elm-3d
> A 3D game engine for Elm

## Local setup 

```
elm install ryan-haskell/elm-3d
```

## Quick example

```elm
import Elm3d.Camera exposing (Camera)
import Elm3d.Node exposing (Node)
import Elm3d.Program exposing (Program)
import Elm3d.Window

main : Program
main =
    Elm3d.Program.new
        { window = Elm3d.Window.fullscreen
        , camera = camera
        , nodes = [ cube ]
        }

cube : Node
cube =
    let
        update { time } node =
            node
                |> Elm3d.Node.withRotationX (time * 0.05)
    in
    Elm3d.Node.cube { size = 1 }
        |> Elm3d.Node.withRotationY (pi / 4)
        |> Elm3d.Node.withOnUpdate update

camera : Camera
camera =
    Elm3d.Camera.perspective
        { fov = 75
        , near = 0.01
        , far = 1000
        }
        |> Elm3d.Camera.withPositionZ 2.0
```

### Running the example

```
cd example
elm reactor
```