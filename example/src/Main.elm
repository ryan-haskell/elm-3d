module Main exposing (main)

import Elm3d.Camera
import Elm3d.Camera.Isometric
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
    Elm3d.Program.new
        { onAssetsLoaded = AssetsLoaded
        , init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { cameraZoom : Float
    , cameraOffset : Vector2
    , playerPosition : Vector3
    , playerRotation : Float
    , npcPosition : Vector3
    , npcRotation : Float
    , npcSwayAmount : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { cameraZoom = 8
      , cameraOffset = Elm3d.Vector2.new -3 5
      , playerPosition = Elm3d.Vector3.new -6 0 -1
      , playerRotation = pi / 2
      , npcPosition = Elm3d.Vector3.zero
      , npcRotation = 0
      , npcSwayAmount = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AssetsLoaded
    | CameraUpdate Context Camera
    | RunningNpcUpdate Context Node
    | PlayerUpdate Context Node
    | SwayBackAndForth Context Node


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AssetsLoaded ->
            ( model
            , Cmd.none
            )

        CameraUpdate ctx camera ->
            let
                cameraPanRate =
                    1.5

                cameraOffset : Vector2
                cameraOffset =
                    Elm3d.Context.toInputAxis ctx
                        { x = ( Key.KEY_A, Key.KEY_D )
                        , y = ( Key.KEY_S, Key.KEY_W )
                        }
                        |> Elm3d.Vector2.scale (ctx.dt * cameraPanRate)
                        |> Elm3d.Vector2.add (Elm3d.Camera.toOffset camera)

                isMousePressed : Bool
                isMousePressed =
                    Elm3d.Context.isLeftClickPressed ctx

                targetSize : Float
                targetSize =
                    if isMousePressed then
                        4

                    else
                        8

                cameraZoom : Float
                cameraZoom =
                    Elm3d.Float.lerp
                        { from = Elm3d.Camera.Isometric.size camera
                        , to = targetSize
                        , step = ctx.dt * 10
                        }
            in
            ( { model
                | cameraOffset = cameraOffset
                , cameraZoom = cameraZoom
              }
            , Cmd.none
            )

        RunningNpcUpdate ctx node ->
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
            ( { model
                | npcPosition = newPosition
                , npcRotation = ctx.time + pi / 2
              }
            , Cmd.none
            )

        PlayerUpdate ctx node ->
            let
                playerMoveSpeed : Float
                playerMoveSpeed =
                    1.5

                input : Elm3d.Vector2.Vector2
                input =
                    Elm3d.Context.toInputAxis ctx
                        { x = ( KEY_A, KEY_D )
                        , y = ( KEY_S, KEY_W )
                        }
                        |> Elm3d.Vector2.scale (ctx.dt * playerMoveSpeed)

                isNotMoving : Bool
                isNotMoving =
                    Elm3d.Vector2.length input == 0

                position : Vector2
                position =
                    Elm3d.Vector2.new
                        (Elm3d.Node.toPositionX node)
                        (Elm3d.Node.toPositionZ node)

                delta : Vector2
                delta =
                    if isNotMoving then
                        Elm3d.Vector2.zero

                    else
                        Elm3d.Isometric.toOffsetVector
                            { angle = cameraRotation
                            , input = input
                            }

                playerRotation : Float
                playerRotation =
                    if isNotMoving then
                        Elm3d.Node.toRotationY node

                    else
                        Elm3d.Rotation.lerp
                            { from = Elm3d.Node.toRotationY node
                            , to = atan2 (Elm3d.Vector2.x delta) (Elm3d.Vector2.y delta)
                            , step = ctx.dt * 10
                            }

                playerHeight =
                    if isNotMoving then
                        0

                    else
                        abs (bounceHeight * cos (12 * ctx.time))
            in
            ( { model
                | playerPosition =
                    Elm3d.Vector3.new
                        (Elm3d.Vector2.x position + Elm3d.Vector2.x delta)
                        playerHeight
                        (Elm3d.Vector2.y position + Elm3d.Vector2.y delta)
                , playerRotation = playerRotation
              }
            , Cmd.none
            )

        SwayBackAndForth ctx node ->
            ( { model | npcSwayAmount = 0.05 * cos (12 * ctx.time) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
        ]
    }


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
        |> Elm3d.Node.withPositionZ -8


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
        |> Elm3d.Node.withOnUpdate RunningNpcUpdate
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
    Elm3d.Camera.Isometric.new
        { size = model.cameraZoom
        , near = 0.01
        , far = 1000
        , rotation = cameraRotation
        , angle = pi / 6
        , distance = 100
        , offset = model.cameraOffset
        }
        |> Elm3d.Camera.withOnUpdate CameraUpdate


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
        |> Elm3d.Node.withOnUpdate PlayerUpdate



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
        |> Elm3d.Node.withOnUpdate SwayBackAndForth


bounceHeight : Float
bounceHeight =
    0.05
