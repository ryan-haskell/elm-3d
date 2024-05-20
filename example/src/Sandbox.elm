module Sandbox exposing (main)

import Elm3d.Camera
import Elm3d.Color exposing (Color)
import Elm3d.Context exposing (Context)
import Elm3d.Float
import Elm3d.Input.Event
import Elm3d.Input.Key as Key exposing (Key(..))
import Elm3d.Isometric
import Elm3d.Node
import Elm3d.Program exposing (Program)
import Elm3d.Rotation
import Elm3d.Texture exposing (Texture)
import Elm3d.Vector2 exposing (Vector2)
import Elm3d.Vector3 exposing (Vector3)
import Elm3d.Viewport
import Html exposing (Html)


type alias Flags =
    ()


type alias Node =
    Elm3d.Node.Node Msg


type alias Camera =
    Elm3d.Camera.Camera Msg


main : Program Flags Model Msg
main =
    Elm3d.Program.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    { isIsometricCamera : Bool
    , cameraZoom : Float
    , cameraOffset : Vector2
    , cameraAngle : Float
    , playerPosition : Vector3
    , playerRotation : Float
    , npcPosition : Vector3
    , npcRotation : Float
    , npcSwayAmount : Float
    }


init : Flags -> Model
init flags =
    { isIsometricCamera = False
    , cameraZoom = 8
    , cameraOffset = Elm3d.Vector2.new -3 5
    , cameraAngle = 0
    , playerPosition = Elm3d.Vector3.new -6 0 -1
    , playerRotation = pi / 2
    , npcPosition = Elm3d.Vector3.zero
    , npcRotation = 0
    , npcSwayAmount = 0
    }



-- UPDATE


type Msg
    = CameraUpdate Context
    | RunningNpcUpdate Context
    | PlayerUpdate Context
    | SwayBackAndForth Context


update : Msg -> Model -> Model
update msg model =
    case msg of
        CameraUpdate ctx ->
            let
                cameraPanRate =
                    1.5

                cameraZoomRate =
                    15.0

                cameraOffset : Vector2
                cameraOffset =
                    Elm3d.Context.toInputVector ctx
                        { x = ( KEY_A, KEY_D )
                        , y = ( KEY_S, KEY_W )
                        }
                        |> Elm3d.Vector2.scale (ctx.dt * cameraPanRate)
                        |> Elm3d.Vector2.add model.cameraOffset

                isMousePressed : Bool
                isMousePressed =
                    Elm3d.Context.isLeftClickPressed ctx

                targetSize : Float
                targetSize =
                    if isMousePressed then
                        5

                    else
                        8

                cameraZoom : Float
                cameraZoom =
                    Elm3d.Float.lerp
                        { from = model.cameraZoom
                        , to = targetSize
                        , step = ctx.dt * cameraZoomRate
                        }

                cameraAngleInput =
                    Elm3d.Context.toInputAxis ctx
                        ( KEY_ARROW_LEFT, KEY_ARROW_RIGHT )

                cameraAngle =
                    model.cameraAngle + (ctx.dt * cameraAngleInput)
            in
            { model
                | cameraOffset = cameraOffset
                , cameraZoom = cameraZoom
                , cameraAngle = cameraAngle
            }

        RunningNpcUpdate ctx ->
            let
                radius : Float
                radius =
                    0.9

                newPosition =
                    Elm3d.Vector3.new
                        (radius * sin ctx.time)
                        (abs (bounceHeight * cos (12 * ctx.time)))
                        (radius * cos ctx.time)
            in
            { model
                | npcPosition = newPosition
                , npcRotation = ctx.time + pi / 2
            }

        PlayerUpdate ctx ->
            let
                playerMoveSpeed : Float
                playerMoveSpeed =
                    1.5

                input : Elm3d.Vector2.Vector2
                input =
                    Elm3d.Context.toInputVector ctx
                        { x = ( KEY_A, KEY_D )
                        , y =
                            if model.isIsometricCamera then
                                ( KEY_S, KEY_W )

                            else
                                ( KEY_W, KEY_S )
                        }
                        |> Elm3d.Vector2.scale (ctx.dt * playerMoveSpeed)
                        |> (if model.isIsometricCamera then
                                identity

                            else
                                Elm3d.Vector2.rotate model.cameraAngle
                           )

                isNotMoving : Bool
                isNotMoving =
                    Elm3d.Vector2.length input == 0

                position : Vector2
                position =
                    Elm3d.Vector2.new
                        (Elm3d.Vector3.x model.playerPosition)
                        (Elm3d.Vector3.z model.playerPosition)

                delta : Vector2
                delta =
                    if isNotMoving then
                        Elm3d.Vector2.zero

                    else if model.isIsometricCamera then
                        Elm3d.Isometric.toOffsetVector
                            { angle = cameraRotation
                            , input = input
                            }

                    else
                        input

                playerRotation : Float
                playerRotation =
                    if isNotMoving then
                        model.playerRotation

                    else
                        Elm3d.Rotation.lerp
                            { from = model.playerRotation
                            , to = atan2 (Elm3d.Vector2.x delta) (Elm3d.Vector2.y delta)
                            , step = ctx.dt * 10
                            }

                playerHeight =
                    if isNotMoving then
                        0

                    else
                        abs (bounceHeight * cos (12 * ctx.time))
            in
            { model
                | playerPosition =
                    Elm3d.Vector3.new
                        (Elm3d.Vector2.x position + Elm3d.Vector2.x delta)
                        playerHeight
                        (Elm3d.Vector2.y position + Elm3d.Vector2.y delta)
                , playerRotation = playerRotation
            }

        SwayBackAndForth ctx ->
            { model | npcSwayAmount = 0.05 * cos (12 * ctx.time) }



-- VIEW


view : Model -> Elm3d.Program.View Msg
view model =
    { viewport = Elm3d.Viewport.fullscreenAspect (16 / 9)
    , background = skyBlue
    , camera = toCamera model
    , nodes =
        [ ground
        , trees
        , npcRunningAroundTavern model
        , church
        , beardedGuy model
        , marketArea model
        ]
    }


market : Node
market =
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/building_market_blue.obj"
        }
        |> Elm3d.Node.withRotationY (pi / 4)


marketArea model =
    Elm3d.Node.group
        [ market
        , vendor model
        ]
        |> Elm3d.Node.withPosition (Elm3d.Vector3.new -5.5 0 -5.5)


vendor model =
    Elm3d.Node.group
        [ toVillager model { name = "male1" }
        ]
        |> Elm3d.Node.withRotationY (pi / 3)
        |> Elm3d.Node.withPosition (Elm3d.Vector3.new 0 0 1)


skyBlue : Color
skyBlue =
    Elm3d.Color.rgb 0 191 255


ground : Node
ground =
    Elm3d.Node.block
        { size = Elm3d.Vector3.new 50 2 50
        , texture = Elm3d.Texture.rgb 0 191 128
        }
        |> Elm3d.Node.withPositionY -1



-- LADY & TAVERN


npcRunningAroundTavern : Model -> Node
npcRunningAroundTavern model =
    Elm3d.Node.group
        [ tavern
        , runningNpc model
        ]
        |> Elm3d.Node.withPositionX -3
        |> Elm3d.Node.withPositionZ -6


tavern : Node
tavern =
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/building_tavern_blue.obj"
        }


runningNpc : Model -> Node
runningNpc model =
    Elm3d.Node.group
        [ toVillager model { name = "female1" }
        ]
        |> Elm3d.Node.withOnFrame RunningNpcUpdate
        |> Elm3d.Node.withPosition model.npcPosition
        |> Elm3d.Node.withRotationY model.npcRotation



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
        |> Elm3d.Node.withScale (Elm3d.Vector3.fromFloat 8)
        |> Elm3d.Node.withPositionX (16 * cos angle)
        |> Elm3d.Node.withPositionZ (16 * sin angle)



-- CAMERA


toCamera : Model -> Camera
toCamera model =
    if model.isIsometricCamera then
        isometricCamera model
            |> Elm3d.Camera.withOnFrame CameraUpdate

    else
        perspectiveCamera model
            |> Elm3d.Camera.withOnFrame CameraUpdate


perspectiveCamera : Model -> Camera
perspectiveCamera model =
    let
        cameraZoom =
            2

        offset =
            cameraZoom / 1.5
    in
    Elm3d.Camera.perspective
        { fov = 60
        , range = ( 0.01, 1000 )
        }
        |> Elm3d.Camera.withPosition
            (Elm3d.Vector3.add model.playerPosition
                (Elm3d.Vector3.new
                    (offset * sin -model.cameraAngle)
                    0
                    (offset * cos -model.cameraAngle)
                )
            )
        |> Elm3d.Camera.withPositionY (cameraZoom / 2)
        |> Elm3d.Camera.withRotationX (-pi / 8)
        |> Elm3d.Camera.withRotationY -model.cameraAngle


isometricCamera : Model -> Camera
isometricCamera model =
    Elm3d.Camera.isometric
        { size = model.cameraZoom
        , range = ( 0.01, 1000 )
        , rotation = cameraRotation
        , angle = pi / 6
        , distance = 100
        , offset = model.cameraOffset
        }


cameraRotation : Float
cameraRotation =
    pi / 4


cameraPanSpeed : Float
cameraPanSpeed =
    1.5



-- PLAYER MOVEMENT


beardedGuy : Model -> Node
beardedGuy model =
    Elm3d.Node.group
        [ toVillager model { name = "male2" }
        ]
        |> Elm3d.Node.withPosition model.playerPosition
        |> Elm3d.Node.withRotationY model.playerRotation
        |> Elm3d.Node.withOnFrame PlayerUpdate



-- VILLAGERS & STUFF


swayBackAndForth : Elm3d.Node.Context -> Node -> ( Node, Cmd Msg )
swayBackAndForth { time } node =
    ( node
    , Cmd.none
    )


toVillager : Model -> { name : String } -> Node
toVillager model { name } =
    Elm3d.Node.obj
        { url = "/assets/villagers/character_villager_" ++ name ++ ".obj"
        }
        |> Elm3d.Node.withScale (Elm3d.Vector3.fromFloat 0.2)
        |> Elm3d.Node.withRotationZ model.npcSwayAmount
        |> Elm3d.Node.withOnFrame SwayBackAndForth


bounceHeight : Float
bounceHeight =
    0.05
