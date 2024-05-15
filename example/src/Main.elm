module Main exposing (main)

import Elm3d.Camera
import Elm3d.Camera.Isometric
import Elm3d.Color exposing (Color)
import Elm3d.Context
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
    Elm3d.Node.Node Model Msg


type alias Camera =
    Elm3d.Camera.Camera Model Msg


main : Program Model Msg
main =
    Elm3d.Program.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , camera = camera
        , nodes =
            [ ground
            , trees
            , ladyRunningAroundTavern
            , church
            , beardedGuy
            ]
        }


type alias Model =
    { zoom : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { zoom = 8 }
    , Cmd.none
    )


type Msg
    = Zoom Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Zoom delta ->
            ( { model
                | zoom =
                    (model.zoom + delta)
                        |> clamp 4 12
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Elm3d.Program.View Msg
view model =
    { viewport = Elm3d.Viewport.fullscreenAspect (16 / 9)
    , background = Elm3d.Color.transparent
    , hud = Html.text ""
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
        [ tavern
        , runningLady
        ]
        |> Elm3d.Node.withPositionX -3
        |> Elm3d.Node.withPositionZ -8


tavern : Node
tavern =
    Elm3d.Node.obj
        { url = "/assets/medieval_hexagon/building_tavern_blue.obj"
        }


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
        |> Elm3d.Node.withScale (Elm3d.Vector3.fromFloat 8)
        |> Elm3d.Node.withPositionX (16 * cos angle)
        |> Elm3d.Node.withPositionZ (16 * sin angle)



-- CAMERA


camera : Camera
camera =
    Elm3d.Camera.Isometric.new
        { size = 8
        , near = 0.01
        , far = 1000
        , rotation = cameraRotation
        , angle = pi / 6
        , distance = 100
        , offset = Elm3d.Vector2.new -2 5
        }
        |> Elm3d.Camera.withOnUpdate onCameraUpdate


cameraRotation : Float
cameraRotation =
    pi / 4


cameraPanSpeed : Float
cameraPanSpeed =
    1.5


onCameraUpdate : Elm3d.Node.Context Model -> Camera -> ( Camera, Cmd Msg )
onCameraUpdate ctx cam =
    let
        input : Vector2
        input =
            Elm3d.Context.toInputAxis ctx
                { x = ( Key.KEY_A, Key.KEY_D )
                , y = ( Key.KEY_S, Key.KEY_W )
                }
                |> Elm3d.Vector2.scale (ctx.dt * cameraPanSpeed)

        isMousePressed : Bool
        isMousePressed =
            Elm3d.Context.isLeftClickPressed ctx

        targetSize : Float
        targetSize =
            if isMousePressed then
                4

            else
                8
    in
    ( cam
        |> Elm3d.Camera.Isometric.move input
        |> Elm3d.Camera.withSize
            (Elm3d.Float.lerp
                { from = Elm3d.Camera.Isometric.size cam
                , to = targetSize
                , step = ctx.dt * 10
                }
            )
    , Cmd.none
    )



-- PLAYER MOVEMENT


beardedGuy : Node
beardedGuy =
    Elm3d.Node.group
        [ toVillager { name = "male2" }
        ]
        |> Elm3d.Node.withPosition (Elm3d.Vector3.new -5.5 0 -1.5)
        |> Elm3d.Node.withRotationY (pi / 2)
        |> Elm3d.Node.withOnUpdate onPlayerUpdate


playerMoveSpeed : Float
playerMoveSpeed =
    1.5


onPlayerUpdate : Elm3d.Node.Context Model -> Node -> ( Node, Cmd Msg )
onPlayerUpdate ctx node =
    let
        input : Elm3d.Vector2.Vector2
        input =
            Elm3d.Context.toInputAxis ctx
                { x = ( KEY_ARROW_LEFT, KEY_ARROW_RIGHT )
                , y = ( KEY_ARROW_DOWN, KEY_ARROW_UP )
                }
                |> Elm3d.Vector2.scale (ctx.dt * playerMoveSpeed)

        offset : { x : Float, y : Float }
        offset =
            if Elm3d.Vector2.length input == 0 then
                { x = 0
                , y = 0
                }

            else
                Elm3d.Isometric.toOffsetVector
                    { angle = cameraRotation
                    , input = input
                    }
                    |> Elm3d.Vector2.toRecord

        withWalkingAnimation : Node -> Node
        withWalkingAnimation player =
            if Elm3d.Vector2.length input == 0 then
                player

            else
                player
                    |> Elm3d.Node.withPositionY (abs (bounceHeight * cos (12 * ctx.time)))
                    |> Elm3d.Node.withRotationY
                        (Elm3d.Rotation.lerp
                            { from = Elm3d.Node.toRotationY player
                            , to = atan2 offset.x offset.y
                            , step = ctx.dt * 10
                            }
                        )
    in
    ( node
        |> Elm3d.Node.moveX offset.x
        |> Elm3d.Node.moveZ offset.y
        |> withWalkingAnimation
    , Cmd.none
    )



-- VILLAGERS & STUFF


swayBackAndForth : Elm3d.Node.Context Model -> Node -> ( Node, Cmd Msg )
swayBackAndForth { time } node =
    ( node
        |> Elm3d.Node.withRotationZ (0.05 * cos (12 * time))
    , Cmd.none
    )


bounceHeight : Float
bounceHeight =
    0.05


runInACircle : Elm3d.Node.Context Model -> Node -> ( Node, Cmd Msg )
runInACircle { time } node =
    let
        radius : Float
        radius =
            0.9

        newPosition =
            Elm3d.Vector3.new
                (radius * sin time)
                (abs (bounceHeight * cos (12 * time)))
                (radius * cos time)
    in
    ( node
        |> Elm3d.Node.withPosition newPosition
        |> Elm3d.Node.withRotationY (time + pi / 2)
    , Cmd.none
    )


toVillager : { name : String } -> Node
toVillager { name } =
    Elm3d.Node.obj
        { url = "/assets/villagers/character_villager_" ++ name ++ ".obj"
        }
        |> Elm3d.Node.withScale (Elm3d.Vector3.fromFloat 0.2)
        |> Elm3d.Node.withOnUpdate swayBackAndForth
