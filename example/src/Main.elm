module Main exposing (main)

import Elm3d.Camera exposing (Camera)
import Elm3d.Color exposing (Color)
import Elm3d.Node exposing (Node)
import Elm3d.Program exposing (Program)
import Elm3d.Texture exposing (Texture)
import Elm3d.Vector2
import Elm3d.Vector3
import Elm3d.Viewport


main : Program
main =
    Elm3d.Program.new
        { viewport = Elm3d.Viewport.fullscreenAspect (16 / 9)
        , background = Elm3d.Color.transparent
        , camera =
            Elm3d.Camera.orthographic
                { size = 20
                , near = 0.01
                , far = 1000
                }
                |> Elm3d.Camera.withIsometricTransform
                    { horizontalAngle = pi / 4
                    , verticalAngle = pi / 4
                    , distance = 50
                    , offset = Elm3d.Vector2.new 5 -5
                    }
        , nodes =
            [ tavern
            , ground
            , trees
            ]
        }


doFlips { dt } node =
    node



-- BACKGROUND


skyBlue : Color
skyBlue =
    Elm3d.Color.rgb 0 191 255



-- GRASS


ground : Node
ground =
    Elm3d.Node.block
        { size = Elm3d.Vector3.new 50 2 50
        , texture = Elm3d.Texture.rgb 0 191 128
        }
        |> Elm3d.Node.withPositionY -1



-- BUILDINGS


buildings : Node
buildings =
    Elm3d.Node.group
        [ tavern
            |> Elm3d.Node.withPositionX 1
        , church
            |> Elm3d.Node.withPositionX -1
            |> Elm3d.Node.withPositionZ 0.5
            |> Elm3d.Node.withRotationY (pi / 3)
            |> Elm3d.Node.withScale (Elm3d.Vector3.new 1.5 1.5 1.5)
        ]


tavern : Node
tavern =
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/building_tavern_blue.obj"
        }


church : Node
church =
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/building_church_blue.obj"
        }



-- TREES


treeCount : Int
treeCount =
    8


trees : Node
trees =
    List.range 1 treeCount
        |> List.map toTreeNode
        |> Elm3d.Node.group


toTreeNode : Int -> Node
toTreeNode offset =
    let
        angle : Float
        angle =
            pi * 2 * toFloat offset / toFloat treeCount
    in
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/trees_A_large.obj"
        }
        |> Elm3d.Node.withScale (Elm3d.Vector3.new 8 8 8)
        |> Elm3d.Node.withPositionX (16 * cos angle)
        |> Elm3d.Node.withPositionZ (16 * sin angle)



-- CAMERA


camera : Camera
camera =
    Elm3d.Camera.orthographic
        { size = 10
        , near = 0.01
        , far = 1000
        }
        |> Elm3d.Camera.withOnUpdate
            (\{ dt, time } node ->
                node
                    |> Elm3d.Camera.withPositionY 20.0
                    |> Elm3d.Camera.withPositionX 20.0
                    |> Elm3d.Camera.withPositionZ 20.0
                    |> Elm3d.Camera.withRotationX (-pi / 8)
                    |> Elm3d.Camera.withRotationY (pi / 16)
            )



-- |> Elm3d.Camera.withRotationY 0
-- |> Elm3d.Camera.withOnUpdate spin


cameraDistance : Float
cameraDistance =
    500


cameraHeight : Float
cameraHeight =
    2.0


spin : Elm3d.Node.Context -> Camera -> Camera
spin { time } camera_ =
    let
        speed =
            2
    in
    camera_
        |> Elm3d.Camera.withPositionX (cameraDistance * sin (time * speed))
        |> Elm3d.Camera.withPositionZ (cameraDistance * cos (time * speed))
        |> Elm3d.Camera.withRotationY (time * speed)



-- VILLAGERS


villagers : Node
villagers =
    Elm3d.Node.group
        [ runningLady
        , beardedGuy
            |> Elm3d.Node.withPositionX 1.25
            |> Elm3d.Node.withPositionZ 0.75
            |> Elm3d.Node.withRotationY (-pi / 8)
        ]


beardedGuy : Node
beardedGuy =
    toVillager { name = "male2" }


runningLady : Node
runningLady =
    let
        ladyMesh : Node
        ladyMesh =
            toVillager { name = "female1" }
                |> Elm3d.Node.withOnUpdate swayBackAndForth
    in
    Elm3d.Node.group [ ladyMesh ]
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
