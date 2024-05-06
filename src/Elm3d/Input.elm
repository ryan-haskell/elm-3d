module Elm3d.Input exposing
    ( Event
    , Model
    , RawEvent
    , init
    , subscriptions
    , update
    )

import Browser.Events
import Elm3d.Input.Key exposing (Key)
import Json.Decode
import Set exposing (Set)
import Task


type Event
    = KeyPressed Key
    | KeyReleased Key


type RawEvent
    = KeyDown Key
    | KeyUp Key


type Model
    = Model { pressed : Set Int }


init : Model
init =
    Model
        { pressed = Set.empty
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


subscriptions : (RawEvent -> msg) -> Sub msg
subscriptions toMsg =
    Sub.batch
        [ Browser.Events.onKeyUp keyEventDecoder
            |> Sub.map (KeyUp >> toMsg)
        , Browser.Events.onKeyDown keyEventDecoder
            |> Sub.map (KeyDown >> toMsg)
        ]


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
