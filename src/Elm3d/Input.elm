module Elm3d.Input exposing
    ( Model
    , RawEvent
    , init
    , isKeyPressed
    , isLeftClickPressed
    , releaseAllKeys
    , subscriptions
    , update
    )

import Browser.Events
import Elm3d.Input.Event exposing (Event(..))
import Elm3d.Input.Key exposing (Key)
import Elm3d.Input.Mouse exposing (Button)
import Json.Decode
import Set exposing (Set)
import Task


type alias Event =
    Elm3d.Input.Event.Event


type RawEvent
    = KeyDown Key
    | KeyUp Key
    | MouseUp Button
    | MouseDown Button


type Model
    = Model
        { pressed : Set Int
        , mouse : { left : Bool, right : Bool, middle : Bool }
        }


isKeyPressed : Model -> Key -> Bool
isKeyPressed (Model { pressed }) key =
    Set.member (Elm3d.Input.Key.toCode key) pressed


isLeftClickPressed : Model -> Bool
isLeftClickPressed (Model { mouse }) =
    mouse.left


releaseAllKeys : Model -> Model
releaseAllKeys (Model model) =
    Model { model | pressed = Set.empty }


init : Model
init =
    Model
        { pressed = Set.empty
        , mouse = { left = False, right = False, middle = False }
        }


update :
    { event : RawEvent
    }
    -> Model
    -> ( Model, Maybe Event )
update props (Model state) =
    case props.event of
        KeyDown key ->
            let
                code =
                    Elm3d.Input.Key.toCode key

                isAlreadyPressed =
                    Set.member code state.pressed
            in
            ( Model { state | pressed = Set.insert code state.pressed }
            , if isAlreadyPressed then
                Nothing

              else
                Just (KeyPressed key)
            )

        KeyUp key ->
            let
                code =
                    Elm3d.Input.Key.toCode key

                isAlreadyPressed =
                    Set.member code state.pressed
            in
            ( Model { state | pressed = Set.remove code state.pressed }
            , if isAlreadyPressed then
                Just (KeyReleased key)

              else
                Nothing
            )

        MouseUp button ->
            handleMouseButton button
                False
                MouseReleased
                (Model state)

        MouseDown button ->
            handleMouseButton button
                True
                MousePressed
                (Model state)


handleMouseButton :
    Button
    -> Bool
    -> (Button -> Event)
    -> Model
    -> ( Model, Maybe Event )
handleMouseButton button newValue toEvent (Model state) =
    let
        mouse =
            state.mouse
    in
    ( case button of
        Elm3d.Input.Mouse.LeftClick ->
            Model { state | mouse = { mouse | left = newValue } }

        Elm3d.Input.Mouse.RightClick ->
            Model { state | mouse = { mouse | right = newValue } }

        Elm3d.Input.Mouse.MiddleClick ->
            Model { state | mouse = { mouse | middle = newValue } }
    , Just (toEvent button)
    )


subscriptions : (RawEvent -> msg) -> Sub msg
subscriptions toMsg =
    Sub.batch
        [ Browser.Events.onKeyUp keyEventDecoder
            |> Sub.map (KeyUp >> toMsg)
        , Browser.Events.onKeyDown keyEventDecoder
            |> Sub.map (KeyDown >> toMsg)
        , Browser.Events.onMouseUp mouseDecoder
            |> Sub.map (MouseUp >> toMsg)
        , Browser.Events.onMouseDown mouseDecoder
            |> Sub.map (MouseDown >> toMsg)
        ]


mouseDecoder : Json.Decode.Decoder Button
mouseDecoder =
    Json.Decode.field "button" Json.Decode.int
        |> Json.Decode.andThen
            (\button ->
                case button of
                    0 ->
                        Json.Decode.succeed Elm3d.Input.Mouse.LeftClick

                    1 ->
                        Json.Decode.succeed Elm3d.Input.Mouse.RightClick

                    2 ->
                        Json.Decode.succeed Elm3d.Input.Mouse.MiddleClick

                    _ ->
                        Json.Decode.fail "Unrecognized button"
            )


keyEventDecoder : Json.Decode.Decoder Key
keyEventDecoder =
    Json.Decode.field "keyCode" Json.Decode.int
        |> Json.Decode.andThen
            (\code ->
                case Elm3d.Input.Key.fromCode code of
                    Just key ->
                        Json.Decode.succeed key

                    Nothing ->
                        Json.Decode.fail ("Ignoring code: " ++ String.fromInt code)
            )
