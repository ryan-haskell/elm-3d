module Main exposing (main)

import Elm3d.Camera exposing (Camera)
import Elm3d.Color
import Elm3d.Node exposing (Node)
import Elm3d.Program exposing (Program)
import Elm3d.Texture
import Elm3d.Vector3
import Elm3d.Viewport


main : Program
main =
    Elm3d.Program.new
        { background = Elm3d.Color.rgb 0.6 0.9 1
        , viewport = Elm3d.Viewport.fullscreen
        , camera =
            Elm3d.Camera.perspective
                { fov = 60
                , near = 0.01
                , far = 100
                }
                |> Elm3d.Camera.withPosition
                    (Elm3d.Vector3.new 5 0.5 5)
                |> Elm3d.Camera.withRotationX (-pi / 12)
                |> Elm3d.Camera.withRotationY (pi / 4)
                |> Elm3d.Camera.withOnUpdate spin
        , nodes =
            [ ground
            , buildings
            , villagers
            , trees

            -- , Elm3d.Node.light { direction = Elm3d.Vector3.new 1 1 0 }
            ]
        }


trees : Node
trees =
    List.range 1 8
        |> List.map toTreeNode
        |> Elm3d.Node.group


toTreeNode : Int -> Node
toTreeNode offset =
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/trees_A_large.obj"
        }
        |> Elm3d.Node.withScale (Elm3d.Vector3.new 8 8 8)
        |> Elm3d.Node.withPositionX (16 * cos (pi * toFloat offset / 4))
        |> Elm3d.Node.withPositionZ (16 * sin (pi * toFloat offset / 4))
        |> Elm3d.Node.withPositionY -1


villagers : Node
villagers =
    Elm3d.Node.group
        [ runningVillager
        , toVillager { name = "male2" }
            |> Elm3d.Node.withPositionX 1.25
            |> Elm3d.Node.withPositionZ 0.75
            |> Elm3d.Node.withRotationY (-pi / 8)
        ]
        |> Elm3d.Node.withPositionY -0.5


ground : Node
ground =
    Elm3d.Node.cube
        { size = 50
        , texture = Elm3d.Texture.rgb 0 0.75 0.5
        }
        |> Elm3d.Node.withScaleY 0.002
        |> Elm3d.Node.withPositionY -0.55


runningVillager : Node
runningVillager =
    Elm3d.Node.group
        [ toVillager { name = "female1" }
            |> Elm3d.Node.withOnUpdate swayBackAndForth
        ]
        |> Elm3d.Node.withOnUpdate runAroundChurch


swayBackAndForth : Elm3d.Node.Context -> Node -> Node
swayBackAndForth { time } node =
    node
        |> Elm3d.Node.withRotationZ (0.15 * sin (8 * time))


runAroundChurch : Elm3d.Node.Context -> Node -> Node
runAroundChurch { time } node =
    let
        churchPos =
            { x = -1
            , z = 0.5
            }

        distance =
            1.2
    in
    node
        |> Elm3d.Node.withPositionX (distance * sin time + churchPos.x)
        |> Elm3d.Node.withPositionZ (distance * cos time + churchPos.z)
        |> Elm3d.Node.withRotationY (time + pi / 2)
        |> Elm3d.Node.withPositionY (abs (0.1 * cos (8 * time)))


toVillager : { name : String } -> Node
toVillager { name } =
    Elm3d.Node.obj
        { url = "/assets/villagers/character_villager_" ++ name ++ ".obj"
        }
        |> Elm3d.Node.withScale (Elm3d.Vector3.new 0.2 0.2 0.2)


buildings : Node
buildings =
    Elm3d.Node.group
        [ tavern
        , church
        ]
        |> Elm3d.Node.withPositionY -0.5


tavern : Node
tavern =
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/building_tavern_blue.obj"
        }
        |> Elm3d.Node.withPositionX 1


church : Node
church =
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/building_church_blue.obj"
        }
        |> Elm3d.Node.withPositionX -1
        |> Elm3d.Node.withPositionZ 0.5
        |> Elm3d.Node.withRotationY (pi / 3)
        |> Elm3d.Node.withScale (Elm3d.Vector3.new 1.5 1.5 1.5)


rotateEveryFrame : Elm3d.Node.Context -> Node -> Node
rotateEveryFrame { dt } node =
    node
        |> Elm3d.Node.rotateY (dt * 0.5)


spin : Elm3d.Node.Context -> Camera -> Camera
spin { time } camera =
    let
        speed =
            0.15
    in
    camera
        |> Elm3d.Camera.withPositionZ (5 * cos (time * speed))
        |> Elm3d.Camera.withPositionX (5 * sin (time * speed))
        |> Elm3d.Camera.withRotationY (time * speed)
