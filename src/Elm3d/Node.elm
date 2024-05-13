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
    , Context, withOnUpdate
    , withOnInput
    , toPosition, toRotation, toScale
    , toRotationX, toRotationY, toRotationZ
    , toEntity, update, toObjFileUrls, onInput, camera, hasUpdateFunction, toCameraProjection, toTransform3d, isDirectionalLight
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

@docs Context, withOnUpdate
@docs withOnInput


### **Accessing data**

@docs toPosition, toRotation, toScale
@docs toPositionX, toPositionY, toPositionZ
@docs toRotationX, toRotationY, toRotationZ
@docs toScaleX, toScaleY, toScaleZ


### TODO: Elm3d.Internals.Node

@docs toEntity, update, toObjFileUrls, onInput, camera, hasUpdateFunction, toCameraProjection, toTransform3d, isDirectionalLight

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


type Node
    = Node Internals


type Kind
    = Block { size : Vector3, texture : Texture }
    | Obj { url : String }
    | Group (List Node)
    | DirectionalLight
    | Camera { projection : Projection }


type alias Internals =
    { kind : Kind
    , transform : Transform3d
    , onInput : Maybe (Elm3d.Input.Event.Event -> Node -> Node)
    , onUpdate : Maybe (Context -> Node -> Node)
    }


toTransform3d : Node -> Transform3d
toTransform3d (Node node) =
    node.transform


toCameraProjection : Node -> Maybe Projection
toCameraProjection (Node node) =
    case node.kind of
        Camera { projection } ->
            Just projection

        _ ->
            Nothing


cube :
    { size : Float
    , texture : Texture
    }
    -> Node
cube props =
    block
        { size = Elm3d.Vector3.fromFloat props.size
        , texture = props.texture
        }


block :
    { size : Vector3
    , texture : Texture
    }
    -> Node
block props =
    Node
        { kind = Block props
        , onInput = Nothing
        , onUpdate = Nothing
        , transform = Elm3d.Transform3d.none
        }


obj : { url : String } -> Node
obj props =
    Node
        { kind = Obj props
        , onInput = Nothing
        , onUpdate = Nothing
        , transform = Elm3d.Transform3d.none
        }


group : List Node -> Node
group children =
    Node
        { kind = Group children
        , onInput = Nothing
        , onUpdate = Nothing
        , transform = Elm3d.Transform3d.none
        }


camera : { projection : Projection } -> Node
camera props =
    Node
        { kind = Camera props
        , onInput = Nothing
        , onUpdate = Nothing
        , transform = Elm3d.Transform3d.none
        }


light : { direction : Vector3 } -> Node
light props =
    Node
        { kind = DirectionalLight
        , onInput = Nothing
        , onUpdate = Nothing
        , transform =
            Elm3d.Transform3d.none
                |> Elm3d.Transform3d.withRotation props.direction
        }


withPosition : Vector3 -> Node -> Node
withPosition props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPosition props node.transform }


withRotation : Vector3 -> Node -> Node
withRotation props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotation props node.transform }


withScale : Vector3 -> Node -> Node
withScale props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withScale props node.transform }


withPositionX : Float -> Node -> Node
withPositionX props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPositionX props node.transform }


withPositionY : Float -> Node -> Node
withPositionY props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPositionY props node.transform }


withPositionZ : Float -> Node -> Node
withPositionZ props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPositionZ props node.transform }


withRotationX : Float -> Node -> Node
withRotationX props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationX props node.transform }


withRotationY : Float -> Node -> Node
withRotationY props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationY props node.transform }


withRotationZ : Float -> Node -> Node
withRotationZ props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withRotationZ props node.transform }


withScaleX : Float -> Node -> Node
withScaleX props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withScaleX props node.transform }


withScaleY : Float -> Node -> Node
withScaleY props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withScaleY props node.transform }


withScaleZ : Float -> Node -> Node
withScaleZ props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withScaleZ props node.transform }


moveX : Float -> Node -> Node
moveX delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withPositionX
                    (delta + Elm3d.Transform3d.toPositionX node.transform)
                    node.transform
        }


moveY : Float -> Node -> Node
moveY delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withPositionY
                    (delta + Elm3d.Transform3d.toPositionY node.transform)
                    node.transform
        }


moveZ : Float -> Node -> Node
moveZ delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withPositionZ
                    (delta + Elm3d.Transform3d.toPositionZ node.transform)
                    node.transform
        }


rotateX : Float -> Node -> Node
rotateX delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withRotationX
                    (delta + Elm3d.Transform3d.toRotationX node.transform)
                    node.transform
        }


rotateY : Float -> Node -> Node
rotateY delta (Node node) =
    Node
        { node
            | transform =
                Elm3d.Transform3d.withRotationY
                    (delta + Elm3d.Transform3d.toRotationY node.transform)
                    node.transform
        }


rotateZ : Float -> Node -> Node
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


withOnUpdate : (Context -> Node -> Node) -> Node -> Node
withOnUpdate props (Node node) =
    Node { node | onUpdate = Just props }


withOnInput : (Elm3d.Input.Event.Event -> Node -> Node) -> Node -> Node
withOnInput props (Node node) =
    Node { node | onInput = Just props }



-- READING


isDirectionalLight : Node -> Bool
isDirectionalLight (Node node) =
    case node.kind of
        DirectionalLight ->
            True

        _ ->
            False


toPosition : Node -> Vector3
toPosition (Node node) =
    Elm3d.Transform3d.toPosition node.transform


toRotation : Node -> Vector3
toRotation (Node node) =
    Elm3d.Transform3d.toRotation node.transform


toScale : Node -> Vector3
toScale (Node node) =
    Elm3d.Transform3d.toScale node.transform


toRotationX : Node -> Float
toRotationX (Node node) =
    Elm3d.Transform3d.toRotationX node.transform


toRotationY : Node -> Float
toRotationY (Node node) =
    Elm3d.Transform3d.toRotationY node.transform


toRotationZ : Node -> Float
toRotationZ (Node node) =
    Elm3d.Transform3d.toRotationZ node.transform


toEntity :
    Matrix4
    ->
        { camera : Matrix4
        , light : Maybe Vector3
        , assets : Elm3d.Asset.Model
        }
    -> Node
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


toObjFileUrls : Node -> List String
toObjFileUrls (Node node) =
    case node.kind of
        Group children ->
            List.concatMap toObjFileUrls children

        Obj { url } ->
            [ url ]

        _ ->
            []


onInput : Elm3d.Input.Event.Event -> Node -> Node
onInput event (Node node) =
    case node.kind of
        Group children ->
            let
                updatedGroupNode : Node
                updatedGroupNode =
                    Node { node | kind = Group (List.map (onInput event) children) }
            in
            case node.onInput of
                Just fn ->
                    fn event updatedGroupNode

                Nothing ->
                    updatedGroupNode

        _ ->
            case node.onInput of
                Just fn ->
                    fn event (Node node)

                Nothing ->
                    Node node


hasUpdateFunction : Node -> Bool
hasUpdateFunction (Node node) =
    case node.kind of
        Group children ->
            thisNodeHasUpdate (Node node) || List.any hasUpdateFunction children

        _ ->
            thisNodeHasUpdate (Node node)


thisNodeHasUpdate : Node -> Bool
thisNodeHasUpdate (Node node) =
    node.onUpdate /= Nothing


update : Context -> Node -> Node
update ctx (Node node) =
    case node.kind of
        Group children ->
            if hasUpdateFunction (Node node) || List.any hasUpdateFunction children then
                case node.onUpdate of
                    Nothing ->
                        Node { node | kind = Group (List.map (update ctx) children) }

                    Just onUpdate ->
                        let
                            (Node updatedGroupNode) =
                                onUpdate ctx (Node node)
                        in
                        Node { updatedGroupNode | kind = Group (List.map (update ctx) children) }

            else
                Node node

        _ ->
            case node.onUpdate of
                Just onUpdate ->
                    onUpdate ctx (Node node)

                Nothing ->
                    Node node
