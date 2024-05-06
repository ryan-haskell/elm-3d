module Elm3d.Asset exposing
    ( Asset(..)
    , Model
    , Msg
    , findObj
    , init
    , update
    )

import Dict exposing (Dict)
import Elm3d.File.Mtl
import Elm3d.File.Obj
import Http
import WebGL.Texture


type Asset
    = Mtl (Loadable Elm3d.File.Mtl.Data)
    | Obj (Loadable Elm3d.File.Obj.Data)
    | Png (Loadable WebGL.Texture.Texture)



-- INIT


type Model
    = Model
        { dict : Dict Url Asset
        }


type alias Url =
    String


type Loadable data
    = Loading
    | Success data
    | Failure Problem


type Problem
    = Http Http.Error
    | ObjInvalid Elm3d.File.Obj.Error


init : { objFileUrls : List String } -> ( Model, Cmd Msg )
init { objFileUrls } =
    ( Model { dict = Dict.empty }
    , Cmd.batch
        [ fetchObjFiles objFileUrls
        ]
    )


fetchObjFiles : List String -> Cmd Msg
fetchObjFiles urls =
    Cmd.batch (List.map fetchObjFile urls)


fetchObjFile : String -> Cmd Msg
fetchObjFile url =
    Http.get
        { url = url
        , expect = Http.expectString (FetchedObjFile url)
        }



-- UPDATE


type Msg
    = FetchedObjFile Url (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        FetchedObjFile url (Ok str) ->
            let
                objFile : Elm3d.File.Obj.Data
                objFile =
                    Elm3d.File.Obj.parse url str

                _ =
                    Debug.log "Success" objFile
            in
            ( Model
                { model
                    | dict = Dict.insert url (Obj (Success objFile)) model.dict
                }
            , -- TODO: Fetch more assets (like MTL files)
              Cmd.none
            )

        FetchedObjFile url (Err httpError) ->
            ( Model { model | dict = Dict.insert url (Obj (Failure (Http httpError))) model.dict }
            , Cmd.none
            )



-- MODEL


findObj : Url -> Model -> Maybe Elm3d.File.Obj.Data
findObj url (Model { dict }) =
    case Dict.get url dict of
        Just (Obj (Success data)) ->
            Just data

        _ ->
            Nothing
