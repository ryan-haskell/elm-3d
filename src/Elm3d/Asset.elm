module Elm3d.Asset exposing
    ( Asset(..)
    , Model
    , Msg
    , findMtl
    , findObj
    , findPng
    , init
    , isLoading
    , update
    )

import Dict exposing (Dict)
import Elm3d.File.Mtl
import Elm3d.File.Obj
import Http
import Set
import Task
import Url exposing (Url)
import WebGL.Texture


type Asset
    = Mtl (Loadable Elm3d.File.Mtl.Data)
    | Obj (Loadable Elm3d.File.Obj.Data)
    | Png (Loadable WebGL.Texture.Texture)



-- INIT


type Model
    = Model
        { dict : Dict String Asset
        }


type Loadable data
    = Loading
    | Success data
    | Failure Problem


type Problem
    = Http Http.Error
    | Texture WebGL.Texture.Error


init : { objFileUrls : List String } -> ( Model, Cmd Msg )
init { objFileUrls } =
    let
        model =
            Model { dict = Dict.empty }
    in
    ( model
    , Cmd.batch
        [ fetchObjFiles model objFileUrls
        ]
    )


fetchObjFiles : Model -> List String -> Cmd Msg
fetchObjFiles model urls =
    Cmd.batch (List.map (fetchObjFile model) urls)


fetchObjFile : Model -> String -> Cmd Msg
fetchObjFile (Model model) url =
    if Dict.member url model.dict then
        Cmd.none

    else
        Http.get
            { url = url
            , expect = Http.expectString (FetchedObjFile url)
            }


fetchMtlFile : Model -> String -> Cmd Msg
fetchMtlFile (Model model) url =
    if Dict.member url model.dict then
        Cmd.none

    else
        Http.get
            { url = url
            , expect = Http.expectString (FetchedMtlFile url)
            }


fetchPngTexture : Model -> String -> Cmd Msg
fetchPngTexture (Model model) url =
    if Dict.member url model.dict then
        Cmd.none

    else
        WebGL.Texture.load url
            |> Task.attempt (FetchedPngTexture url)



-- UPDATE


type Msg
    = FetchedObjFile String (Result Http.Error String)
    | FetchedMtlFile String (Result Http.Error String)
    | FetchedPngTexture String (Result WebGL.Texture.Error WebGL.Texture.Texture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        FetchedObjFile url (Ok str) ->
            let
                objFile : Elm3d.File.Obj.Data
                objFile =
                    Elm3d.File.Obj.parse url str

                mtlUrls : List String
                mtlUrls =
                    objFile.info.mtl
                        |> Set.fromList
                        |> Set.toList
            in
            ( Model
                { model
                    | dict =
                        mtlUrls
                            |> List.foldl (\k -> Dict.insert k (Mtl Loading))
                                (Dict.insert url (Obj (Success objFile)) model.dict)
                }
            , Cmd.batch (List.map (fetchMtlFile (Model model)) mtlUrls)
            )

        FetchedObjFile url (Err httpError) ->
            ( Model { model | dict = Dict.insert url (Obj (Failure (Http httpError))) model.dict }
            , Cmd.none
            )

        FetchedMtlFile url (Ok str) ->
            let
                mtlFile : Elm3d.File.Mtl.Data
                mtlFile =
                    Elm3d.File.Mtl.parse url str

                pngTextureUrls : List String
                pngTextureUrls =
                    Dict.values mtlFile.materials
                        |> List.filterMap .map_Kd
                        |> List.filter (String.endsWith ".png")
                        |> Set.fromList
                        |> Set.toList
            in
            ( Model
                { model
                    | dict =
                        pngTextureUrls
                            |> List.foldl (\k -> Dict.insert k (Png Loading))
                                (model.dict
                                    |> Dict.insert url (Mtl (Success mtlFile))
                                )
                }
            , Cmd.batch (List.map (fetchPngTexture (Model model)) pngTextureUrls)
            )

        FetchedMtlFile url (Err httpError) ->
            ( Model { model | dict = Dict.insert url (Mtl (Failure (Http httpError))) model.dict }
            , Cmd.none
            )

        FetchedPngTexture url (Ok texture) ->
            ( Model { model | dict = Dict.insert url (Png (Success texture)) model.dict }
            , Cmd.none
            )

        FetchedPngTexture url (Err textureError) ->
            ( Model { model | dict = Dict.insert url (Png (Failure (Texture textureError))) model.dict }
            , Cmd.none
            )



-- MODEL


findObj : String -> Model -> Maybe Elm3d.File.Obj.Data
findObj url (Model { dict }) =
    case Dict.get url dict of
        Just (Obj (Success data)) ->
            Just data

        _ ->
            Nothing


findMtl : String -> Model -> Maybe Elm3d.File.Mtl.Data
findMtl url (Model { dict }) =
    case Dict.get url dict of
        Just (Mtl (Success data)) ->
            Just data

        _ ->
            Nothing


findPng : String -> Model -> Maybe WebGL.Texture.Texture
findPng url (Model { dict }) =
    case Dict.get url dict of
        Just (Png (Success data)) ->
            Just data

        _ ->
            Nothing


isLoading : String -> Model -> Bool
isLoading url (Model { dict }) =
    case Dict.get url dict of
        Nothing ->
            False

        Just (Obj loadable) ->
            loadable == Loading

        Just (Mtl loadable) ->
            loadable == Loading

        Just (Png loadable) ->
            loadable == Loading
