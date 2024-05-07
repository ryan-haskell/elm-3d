module Elm3d.Component exposing
    ( Props
    , Model, init
    , Msg, update, subscriptions
    , view
    , toViewportSize
    )

{-|

@docs Props
@docs Model, init
@docs Msg, update, subscriptions
@docs view

@docs toViewportSize

-}

import Browser
import Browser.Dom
import Browser.Events
import Elm3d.Asset
import Elm3d.Camera
import Elm3d.Input
import Elm3d.Matrix4
import Elm3d.Node exposing (Node)
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Task
import WebGL


type Model
    = Model
        { time : Float
        , viewport : ( Int, Int )
        , nodes : List Node
        , input : Elm3d.Input.Model
        , assets : Elm3d.Asset.Model
        }


toViewportSize : Model -> ( Int, Int )
toViewportSize (Model model) =
    model.viewport


type alias Props =
    { camera : Elm3d.Camera.Camera
    , nodes : List Node
    }


init : Props -> () -> ( Model, Cmd Msg )
init { nodes } _ =
    let
        objFileUrls : List String
        objFileUrls =
            List.concatMap Elm3d.Node.toObjFileUrls nodes

        ( assets, assetCmd ) =
            Elm3d.Asset.init { objFileUrls = objFileUrls }
    in
    ( Model
        { time = 0.0
        , viewport = ( 0, 0 )
        , nodes = nodes
        , input = Elm3d.Input.init
        , assets = assets
        }
    , Cmd.batch
        [ Browser.Dom.getViewport
            |> Task.perform Viewport
        , Cmd.map Asset assetCmd
        ]
    )



-- UPDATE


type Msg
    = Frame Float
    | Viewport Browser.Dom.Viewport
    | Resize Int Int
    | Input Elm3d.Input.RawEvent
    | Asset Elm3d.Asset.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Asset assetsMsg ->
            let
                ( assets, assetsCmd ) =
                    -- TODO: Consider dynamic model.nodes here
                    -- to fetch objs not loaded at startup
                    Elm3d.Asset.update assetsMsg model.assets
            in
            ( Model { model | assets = assets }
            , Cmd.map Asset assetsCmd
            )

        Frame ms ->
            let
                dt : Float
                dt =
                    ms / 1000

                time : Float
                time =
                    model.time + dt

                context : Elm3d.Node.Context
                context =
                    { dt = dt
                    , time = time
                    }
            in
            ( Model
                { model
                    | time = time
                    , nodes = List.map (updateNode context) model.nodes
                }
            , Cmd.none
            )

        Viewport { scene } ->
            ( Model
                { model
                    | viewport =
                        ( Basics.floor scene.width
                        , Basics.floor scene.height
                        )
                }
            , Cmd.none
            )

        Resize width height ->
            ( Model { model | viewport = ( width, height ) }
            , Cmd.none
            )

        Input internalEvent ->
            let
                ( input, maybeEvent ) =
                    Elm3d.Input.update
                        { event = internalEvent }
                        model.input

                newNodes : List Node
                newNodes =
                    case maybeEvent of
                        Just event ->
                            List.map (Elm3d.Node.onInput event) model.nodes

                        Nothing ->
                            model.nodes
            in
            ( Model { model | input = input, nodes = newNodes }
            , Cmd.none
            )


updateNode : Elm3d.Node.Context -> Node -> Node
updateNode context node =
    case Elm3d.Node.toUpdateFunction node of
        Just fn ->
            fn context node

        Nothing ->
            node


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if hasAnyUpdateNodes model then
            Browser.Events.onAnimationFrameDelta Frame

          else
            Sub.none
        , Browser.Events.onResize Resize
        , Elm3d.Input.subscriptions Input
        ]


hasAnyUpdateNodes : Model -> Bool
hasAnyUpdateNodes (Model model) =
    List.any Elm3d.Node.hasUpdateFunction model.nodes


view : ( Int, Int ) -> Props -> Model -> Html Msg
view size { camera } (Model model) =
    let
        ( width, height ) =
            size

        directionalLight : Maybe Node
        directionalLight =
            model.nodes
                |> List.Extra.find Elm3d.Node.isDirectionalLight
    in
    model.nodes
        |> List.concatMap
            (Elm3d.Node.toEntity Elm3d.Matrix4.identity
                { camera = Elm3d.Camera.toMatrix4 size camera
                , light = directionalLight |> Maybe.map Elm3d.Node.toRotation
                , assets = model.assets
                }
            )
        |> WebGL.toHtmlWith
            [ WebGL.alpha True
            , WebGL.antialias
            , WebGL.depth 1
            ]
            [ Html.Attributes.width width
            , Html.Attributes.height height
            ]
