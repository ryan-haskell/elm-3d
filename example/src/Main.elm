module Main exposing (main)

import Elm3d.Camera exposing (Camera)
import Elm3d.Color exposing (Color)
import Elm3d.Context
import Elm3d.Input.Event
import Elm3d.Input.Key as Key exposing (Key(..))
import Elm3d.Isometric
import Elm3d.Node exposing (Node)
import Elm3d.Program exposing (Program)
import Elm3d.Rotation
import Elm3d.Texture exposing (Texture)
import Elm3d.Vector2 exposing (Vector2)
import Elm3d.Vector3 exposing (Vector3)
import Elm3d.Viewport


main : Program
main =
    Elm3d.Program.new
        { viewport = Elm3d.Viewport.fullscreenAspect (16 / 9)
        , background = Elm3d.Color.transparent
        , camera = camera
        , nodes =
            [ ground
            , ladyRunningAroundTavern
            , church
            , trees
            , beardedGuy
            ]
        }



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



-- LADY & TAVERN


ladyRunningAroundTavern : Node
ladyRunningAroundTavern =
    Elm3d.Node.group
        [ Elm3d.Node.obj
            { url = "/assets/medieval_hexagon/building_tavern_blue.obj"
            }
        , runningLady
        ]
        |> Elm3d.Node.withPositionX -3
        |> Elm3d.Node.withPositionZ -8


runningLady : Node
runningLady =
    Elm3d.Node.group
        [ toVillager { name = "female1" }
        ]
        |> Elm3d.Node.withOnUpdate runInACircle



-- CHURCH


church : Node
church =
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/building_church_blue.obj"
        }
        |> Elm3d.Node.withScale (Elm3d.Vector3.fromFloat 2)
        |> Elm3d.Node.rotateY (pi / 2)
        |> Elm3d.Node.withPositionX -7
        |> Elm3d.Node.withPositionZ -2



-- TREES


treeCount : Int
treeCount =
    12


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
        |> Elm3d.Camera.withIsometricTransform
            { horizontalAngle = cameraRotation
            , verticalAngle = pi / 6
            , distance = 100
            , offset = Elm3d.Vector2.new -1 1
            }
        |> Elm3d.Camera.withOnUpdate onCameraUpdate


cameraRotation : Float
cameraRotation =
    pi / 4


cameraPanSpeed : Float
cameraPanSpeed =
    1.5


onCameraUpdate : Elm3d.Node.Context -> Camera -> Camera
onCameraUpdate ctx cam =
    let
        input : Vector2
        input =
            Elm3d.Context.toInputAxis ctx
                { x = ( Key.KEY_A, Key.KEY_D )
                , y = ( Key.KEY_S, Key.KEY_W )
                }

        offset =
            if Elm3d.Vector2.length input == 0 then
                { x = 0, y = 0 }

            else
                Elm3d.Isometric.toOffsetVector
                    { angle = cameraRotation
                    , input = input
                    }
                    |> Elm3d.Vector2.normalize
                    |> Elm3d.Vector2.scale (ctx.dt * cameraPanSpeed)
                    |> Elm3d.Vector2.toRecord
    in
    cam
        |> Elm3d.Camera.moveX offset.x
        |> Elm3d.Camera.moveZ offset.y



-- PLAYER MOVEMENT


beardedGuy : Node
beardedGuy =
    Elm3d.Node.group
        [ toVillager { name = "male2" }
        ]
        |> Elm3d.Node.withPosition (Elm3d.Vector3.new -5.5 0 -1.5)
        |> Elm3d.Node.withRotationY (pi / 2)
        |> Elm3d.Node.withOnUpdate onPlayerUpdate


playerMoveSpeed =
    1.5


onPlayerUpdate : Elm3d.Node.Context -> Node -> Node
onPlayerUpdate ctx node =
    let
        input : Elm3d.Vector2.Vector2
        input =
            Elm3d.Context.toInputAxis ctx
                { x = ( KEY_ARROW_LEFT, KEY_ARROW_RIGHT )
                , y = ( KEY_ARROW_DOWN, KEY_ARROW_UP )
                }

        offset : { x : Float, y : Float }
        offset =
            if Elm3d.Vector2.length input == 0 then
                { x = 0, y = 0 }

            else
                Elm3d.Isometric.toOffsetVector
                    { angle = cameraRotation
                    , input = input
                    }
                    |> Elm3d.Vector2.normalize
                    |> Elm3d.Vector2.scale (ctx.dt * playerMoveSpeed)
                    |> Elm3d.Vector2.toRecord

        applyMovementTransformations : Node -> Node
        applyMovementTransformations player =
            if Elm3d.Vector2.length input == 0 then
                player

            else
                let
                    currentAngle =
                        Elm3d.Node.toRotationY player

                    targetAngle =
                        atan2 offset.x offset.y
                in
                player
                    |> Elm3d.Node.withPositionY (abs (bounceHeight * cos (12 * ctx.time)))
                    |> Elm3d.Node.withRotationY
                        (Elm3d.Rotation.lerp
                            { from = currentAngle
                            , to = targetAngle
                            , step = ctx.dt * 10
                            }
                        )
    in
    node
        |> Elm3d.Node.moveX offset.x
        |> Elm3d.Node.moveZ offset.y
        |> applyMovementTransformations



-- VILLAGERS & STUFF


swayBackAndForth : Elm3d.Node.Context -> Node -> Node
swayBackAndForth { time } node =
    node
        |> Elm3d.Node.withRotationZ (0.05 * cos (12 * time))


bounceHeight : Float
bounceHeight =
    0.05


runInACircle : Elm3d.Node.Context -> Node -> Node
runInACircle { time } node =
    let
        churchPos =
            { x = 0
            , z = 0
            }

        distance =
            0.9

        newPosition =
            Elm3d.Vector3.new
                (distance * sin time + churchPos.x)
                (abs (bounceHeight * cos (12 * time)))
                (distance * cos time + churchPos.z)
    in
    node
        |> Elm3d.Node.withPosition newPosition
        |> Elm3d.Node.withRotationY (time + pi / 2)


toVillager : { name : String } -> Node
toVillager { name } =
    Elm3d.Node.obj
        { url = "/assets/villagers/character_villager_" ++ name ++ ".obj"
        }
        |> Elm3d.Node.withScale (Elm3d.Vector3.fromFloat 0.2)
        |> Elm3d.Node.withOnUpdate swayBackAndForth
