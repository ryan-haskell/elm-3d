module Main exposing (main)

import Elm3d.Camera exposing (Camera)
import Elm3d.Color
import Elm3d.Node exposing (Node)
import Elm3d.Program exposing (Program)
import Elm3d.Vector3
import Elm3d.Window


main : Program
main =
    Elm3d.Program.new
        { window = Elm3d.Window.fullscreen
        , nodes =
            [ purpleCube
            , blueCube
            ]
        , camera =
            Elm3d.Camera.orthographic
                { size = 8
                , near = 1
                , far = 9999
                }
                |> Elm3d.Camera.withPosition (Elm3d.Vector3.new 0 0 1000)
        }


purpleCube : Node
purpleCube =
    Elm3d.Node.cube { size = 0.25 }
        |> Elm3d.Node.withTextureColor (Elm3d.Color.rgb 0.5 0 1)


blueCube : Node
blueCube =
    Elm3d.Node.cube { size = 0.5 }
        |> Elm3d.Node.withRotationY (pi / 8)
        |> Elm3d.Node.withTextureColor (Elm3d.Color.rgb 0 0.3 0.9)
        |> Elm3d.Node.withOnUpdate onBlueCubeUpdate


onBlueCubeUpdate : Elm3d.Node.Context -> Node -> Node
onBlueCubeUpdate { time } node =
    let
        speed : Float
        speed =
            0.001

        radius : Float
        radius =
            2
    in
    node
        |> Elm3d.Node.withRotationX (Elm3d.Node.toRotationX node + speed * 8.0)
        |> Elm3d.Node.withPosition
            (Elm3d.Vector3.new
                (cos (time * speed * 1.0))
                (sin (time * speed * 1.0))
                -1
            )
