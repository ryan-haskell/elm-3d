module Buildings exposing (main)

import Elm3d.Camera
import Elm3d.Color
import Elm3d.Node
import Elm3d.Program
import Elm3d.Vector2
import Elm3d.Viewport


main =
    Elm3d.Program.view
        { viewport = Elm3d.Viewport.fullscreen
        , background = Elm3d.Color.black
        , camera =
            Elm3d.Camera.isometric
                { size = 6
                , incline = pi / 8
                , spin = pi / 4
                , distance = 5
                , range = ( 1, 10 )
                , offset = Elm3d.Vector2.zero
                }
        , nodes =
            [ Elm3d.Node.obj { url = "/assets/market.obj" }
                |> Elm3d.Node.withPositionX -2
            , Elm3d.Node.obj { url = "/assets/church.obj" }
            , Elm3d.Node.obj { url = "/assets/tavern.obj" }
                |> Elm3d.Node.withPositionX 2
            ]
        }
