module Main exposing (main)

import Elm3d.Camera exposing (Camera)
import Elm3d.Color
import Elm3d.Node
import Elm3d.Program exposing (Program)
import Elm3d.Vector3
import Elm3d.Window


main : Program
main =
    Elm3d.Program.new
        { window = Elm3d.Window.fullscreen
        , nodes =
            [ Elm3d.Node.cube { size = 1 }
                |> Elm3d.Node.withTextureColor (Elm3d.Color.rgb 0.5 0 1)
            , Elm3d.Node.cube { size = 1 }
                |> Elm3d.Node.withTextureColor (Elm3d.Color.rgb 0 0.3 0.9)
                |> Elm3d.Node.withPosition (Elm3d.Vector3.new 1 1 0)
            ]
        , camera =
            Elm3d.Camera.orthographic
                { size = 8
                , near = 1
                , far = 1000
                }
                |> Elm3d.Camera.withPosition (Elm3d.Vector3.new 0 0 1000)
        }
