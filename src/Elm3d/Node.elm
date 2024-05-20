module Elm3d.Node exposing
    ( Node
    , cube, block, obj, light
    , group
    , withPosition, withRotation, withScale
    , withPositionX, withPositionY, withPositionZ
    , withRotationX, withRotationY, withRotationZ
    , withScaleX, withScaleY, withScaleZ
    , moveX, moveY, moveZ
    , rotateX, rotateY, rotateZ
    , Context, withOnFrame
    , withOnInput
    , map
    , toPosition, toRotation, toScale
    , toPositionX, toPositionY, toPositionZ
    , toRotationX, toRotationY, toRotationZ
    , toScaleX, toScaleY, toScaleZ
    , toEntity, update, toObjFileUrls, onInput, camera, hasUpdateFunction, updateProjection, toCameraProjection, toTransform3d, isDirectionalLight
    )

{-|

@docs Node


### **Creating nodes**

@docs cube, block, obj, light
@docs group


### **Moving, rotating, scaling**

@docs withPosition, withRotation, withScale
@docs withPositionX, withPositionY, withPositionZ
@docs withRotationX, withRotationY, withRotationZ
@docs withScaleX, withScaleY, withScaleZ
@docs moveX, moveY, moveZ
@docs rotateX, rotateY, rotateZ


### **Updating nodes**

@docs Context, withOnFrame
@docs withOnInput
@docs map


### **Accessing data**

@docs toPosition, toRotation, toScale
@docs toPositionX, toPositionY, toPositionZ
@docs toRotationX, toRotationY, toRotationZ
@docs toScaleX, toScaleY, toScaleZ


### TODO: Elm3d.Internals.Node

@docs toEntity, update, toObjFileUrls, onInput, camera, hasUpdateFunction, updateProjection, toCameraProjection, toTransform3d, isDirectionalLight

-}

import Elm3d.Asset
import Elm3d.Camera.Projection exposing (Projection(..))
import Elm3d.Color exposing (Color)
import Elm3d.Context
import Elm3d.Entities.Block.TextureColor
import Elm3d.Entities.Obj
import Elm3d.Input.Event
import Elm3d.Input.Key
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Texture exposing (Texture)
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector3 exposing (Vector3)
import Elm3d.Vector4 exposing (Vector4)
import Math.Matrix4
import WebGL
import WebGL.Texture


type Node msg
    = Node (Internals msg)


map : (a -> b) -> Node a -> Node b
map fn (Node node) =
    Node (mapInternals fn node)


type Kind msg
    = Block { size : Vector3, texture : Texture }
    | Obj { url : String }
    | Group (List (Node msg))
    | DirectionalLight
    | Camera { projection : Projection }


mapKind : (a -> b) -> Kind a -> Kind b
mapKind fn kind =
    case kind of
        Block props ->
            Block props

        Obj props ->
            Obj props

        Group children ->
            Group (List.map (map fn) children)

        DirectionalLight ->
            DirectionalLight

        Camera props ->
            Camera props


type alias Internals msg =
    { kind : Kind msg
    , transform : Transform3d
    , onFrame : Maybe (Context -> msg)
    , onInput : Maybe (Elm3d.Input.Event.Event -> msg)
    }


mapInternals : (a -> b) -> Internals a -> Internals b
mapInternals fn node =
    { kind = mapKind fn node.kind
    , transform = node.transform
    , onFrame = Maybe.map (\onFrame_ ctx -> fn (onFrame_ ctx)) node.onFrame
    , onInput = Maybe.map (\onInput_ event -> fn (onInput_ event)) node.onInput
    }


toTransform3d : Node msg -> Transform3d
toTransform3d (Node node) =
    node.transform


toCameraProjection : Node msg -> Maybe Projection
toCameraProjection (Node node) =
    case node.kind of
        Camera { projection } ->
            Just projection

        _ ->
            Nothing


updateProjection : Projection -> Node msg -> Node msg
updateProjection projection (Node node) =
    case node.kind of
        Camera _ ->
            Node { node | kind = Camera { projection = projection } }

        _ ->
            Node node


cube :
    { size : Float
    , texture : Texture
    }
    -> Node msg
cube props =
    block
        { size = Elm3d.Vector3.fromFloat props.size
        , texture = props.texture
        }


block :
    { size : Vector3
    , texture : Texture
    }
    -> Node msg
block props =
    Node
        { kind = Block props
        , onInput = Nothing
        , onFrame = Nothing
        , transform = Elm3d.Transform3d.none
        }


obj : { url : String } -> Node msg
obj props =
    Node
        { kind = Obj props
        , onInput = Nothing
        , onFrame = Nothing
        , transform = Elm3d.Transform3d.none
        }


group : List (Node msg) -> Node msg
group children =
    Node
        { kind = Group children
        , onInput = Nothing
        , onFrame = Nothing
        , transform = Elm3d.Transform3d.none
        }


camera : { projection : Projection } -> Node msg
camera props =
    Node
        { kind = Camera props
        , onInput = Nothing
        , onFrame = Nothing
        , transform = Elm3d.Transform3d.none
        }


light : { direction : Vector3 } -> Node msg
light props =
    Node
        { kind = DirectionalLight
        , onInput = Nothing
        , onFrame = Nothing
        , transform =
            Elm3d.Transform3d.none
                |> Elm3d.Transform3d.withRotation props.direction
        }


withPosition : Vector3 -> Node msg -> Node msg
withPosition props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPosition props node.transform }


withRotation : Vector3 -> Node msg -> Node msg
withRotation props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotation props node.transform }


withScale : Vector3 -> Node msg -> Node msg
withScale props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withScale props node.transform }


withPositionX : Float -> Node msg -> Node msg
withPositionX props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPositionX props node.transform }


withPositionY : Float -> Node msg -> Node msg
withPositionY props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPositionY props node.transform }


withPositionZ : Float -> Node msg -> Node msg
withPositionZ props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPositionZ props node.transform }


withRotationX : Float -> Node msg -> Node msg
withRotationX props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationX props node.transform }


withRotationY : Float -> Node msg -> Node msg
withRotationY props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationY props node.transform }


withRotationZ : Float -> Node msg -> Node msg
withRotationZ props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationZ props node.transform }


withScaleX : Float -> Node msg -> Node msg
withScaleX props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withScaleX props node.transform }


withScaleY : Float -> Node msg -> Node msg
withScaleY props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withScaleY props node.transform }


withScaleZ : Float -> Node msg -> Node msg
withScaleZ props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withScaleZ props node.transform }


moveX : Float -> Node msg -> Node msg
moveX delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withPositionX
                    (delta + Elm3d.Transform3d.toPositionX node.transform)
                    node.transform
        }


moveY : Float -> Node msg -> Node msg
moveY delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withPositionY
                    (delta + Elm3d.Transform3d.toPositionY node.transform)
                    node.transform
        }


moveZ : Float -> Node msg -> Node msg
moveZ delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withPositionZ
                    (delta + Elm3d.Transform3d.toPositionZ node.transform)
                    node.transform
        }


rotateX : Float -> Node msg -> Node msg
rotateX delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withRotationX
                    (delta + Elm3d.Transform3d.toRotationX node.transform)
                    node.transform
        }


rotateY : Float -> Node msg -> Node msg
rotateY delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withRotationY
                    (delta + Elm3d.Transform3d.toRotationY node.transform)
                    node.transform
        }


rotateZ : Float -> Node msg -> Node msg
rotateZ delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withRotationZ
                    (delta + Elm3d.Transform3d.toRotationZ node.transform)
                    node.transform
        }



-- ON TICK & USER INPUT


type alias Context =
    Elm3d.Context.Context


withOnFrame : (Context -> msg) -> Node msg -> Node msg
withOnFrame props (Node node) =
    Node { node | onFrame = Just props }


withOnInput : (Elm3d.Input.Event.Event -> msg) -> Node msg -> Node msg
withOnInput props (Node node) =
    Node { node | onInput = Just props }



-- READING


isDirectionalLight : Node msg -> Bool
isDirectionalLight (Node node) =
    case node.kind of
        DirectionalLight ->
            True

        _ ->
            False


toPosition : Node msg -> Vector3
toPosition (Node node) =
    Elm3d.Transform3d.toPosition node.transform


toPositionX : Node msg -> Float
toPositionX (Node node) =
    Elm3d.Transform3d.toPositionX node.transform


toPositionY : Node msg -> Float
toPositionY (Node node) =
    Elm3d.Transform3d.toPositionY node.transform


toPositionZ : Node msg -> Float
toPositionZ (Node node) =
    Elm3d.Transform3d.toPositionZ node.transform


toRotation : Node msg -> Vector3
toRotation (Node node) =
    Elm3d.Transform3d.toRotation node.transform


toRotationX : Node msg -> Float
toRotationX (Node node) =
    Elm3d.Transform3d.toRotationX node.transform


toRotationY : Node msg -> Float
toRotationY (Node node) =
    Elm3d.Transform3d.toRotationY node.transform


toRotationZ : Node msg -> Float
toRotationZ (Node node) =
    Elm3d.Transform3d.toRotationZ node.transform


toScale : Node msg -> Vector3
toScale (Node node) =
    Elm3d.Transform3d.toScale node.transform


toScaleX : Node msg -> Float
toScaleX (Node node) =
    Elm3d.Transform3d.toScaleX node.transform


toScaleY : Node msg -> Float
toScaleY (Node node) =
    Elm3d.Transform3d.toScaleY node.transform


toScaleZ : Node msg -> Float
toScaleZ (Node node) =
    Elm3d.Transform3d.toScaleZ node.transform


toEntity :
    Matrix4
    ->
        { camera : Matrix4
        , light : Maybe Vector3
        , assets : Elm3d.Asset.Model
        }
    -> Node msg
    -> List WebGL.Entity
toEntity groupMatrix props ((Node { transform, kind }) as node) =
    case kind of
        Camera _ ->
            []

        DirectionalLight ->
            []

        Group children ->
            List.concatMap
                (toEntity
                    (Math.Matrix4.mul
                        groupMatrix
                        (Elm3d.Transform3d.toMatrix4 transform)
                    )
                    props
                )
                children

        Obj { url } ->
            case Elm3d.Asset.findObj url props.assets of
                Just obj_ ->
                    case
                        ( obj_.mesh
                        , Elm3d.Entities.Obj.findTextures obj_ props.assets
                        )
                    of
                        ( Nothing, _ ) ->
                            []

                        ( _, Nothing ) ->
                            []

                        ( Just mesh, Just [] ) ->
                            [ Elm3d.Entities.Obj.toEntityT0 mesh
                                { modelView = Math.Matrix4.mul groupMatrix (Elm3d.Transform3d.toMatrix4 transform)
                                , camera = props.camera
                                , lightDirection = props.light |> Maybe.withDefault Elm3d.Vector3.zero
                                }
                            ]

                        ( Just mesh, Just (t1 :: []) ) ->
                            [ Elm3d.Entities.Obj.toEntityT1 mesh
                                { modelView = Math.Matrix4.mul groupMatrix (Elm3d.Transform3d.toMatrix4 transform)
                                , camera = props.camera
                                , lightDirection = props.light |> Maybe.withDefault Elm3d.Vector3.zero
                                , texture = t1
                                }
                            ]

                        _ ->
                            -- TODO
                            []

                Nothing ->
                    []

        Block { size, texture } ->
            case texture of
                Elm3d.Texture.Color color_ ->
                    [ Elm3d.Entities.Block.TextureColor.toEntity
                        { scale = size
                        , color = color_
                        , modelView =
                            Math.Matrix4.mul groupMatrix
                                (Elm3d.Transform3d.toMatrix4 transform)
                        , camera = props.camera
                        , lightDirection =
                            props.light
                                |> Maybe.withDefault Elm3d.Vector3.zero
                        }
                    ]


toObjFileUrls : Node msg -> List String
toObjFileUrls (Node node) =
    case node.kind of
        Group children ->
            List.concatMap toObjFileUrls children

        Obj { url } ->
            [ url ]

        _ ->
            []


onInput : Elm3d.Input.Event.Event -> Node msg -> List msg
onInput event (Node node) =
    case node.kind of
        Group children ->
            let
                childMsgs : List msg
                childMsgs =
                    List.concatMap (onInput event) children

                updatedGroupNode : Node msg
                updatedGroupNode =
                    Node { node | kind = Group children }
            in
            case node.onInput of
                Just fn ->
                    fn event :: childMsgs

                Nothing ->
                    childMsgs

        _ ->
            case node.onInput of
                Just fn ->
                    [ fn event ]

                Nothing ->
                    []


hasUpdateFunction : Node msg -> Bool
hasUpdateFunction (Node node) =
    case node.kind of
        Group children ->
            thisNodeHasUpdate (Node node) || List.any hasUpdateFunction children

        _ ->
            thisNodeHasUpdate (Node node)


thisNodeHasUpdate : Node msg -> Bool
thisNodeHasUpdate (Node node) =
    node.onFrame /= Nothing


update : Context -> Node msg -> List msg
update ctx (Node node) =
    case node.kind of
        Group children ->
            let
                childMsgs : List msg
                childMsgs =
                    List.concatMap (update ctx) children
            in
            case node.onFrame of
                Just fn ->
                    fn ctx :: childMsgs

                Nothing ->
                    childMsgs

        _ ->
            case node.onFrame of
                Just onFrame ->
                    [ onFrame ctx ]

                Nothing ->
                    []
