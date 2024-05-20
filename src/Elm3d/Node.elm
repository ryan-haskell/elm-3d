module Elm3d.Node exposing
    ( Node
    , cube, block
    , obj
    , group
    , withPosition
    , withPositionX, withPositionY, withPositionZ
    , withRotation
    , withRotationX, withRotationY, withRotationZ
    , withScale
    , withScaleX, withScaleY, withScaleZ
    , withOnFrame, withOnInput
    , map
    )

{-| This module allows you to define 3D objects, lights, cameras and more for use in an Elm3d program.


# **Creating nodes**

@docs Node
@docs cube, block
@docs obj
@docs group


# **Moving, rotating, scaling**

@docs withPosition
@docs withPositionX, withPositionY, withPositionZ

@docs withRotation
@docs withRotationX, withRotationY, withRotationZ

@docs withScale
@docs withScaleX, withScaleY, withScaleZ


# **Handling events**

@docs withOnFrame, withOnInput
@docs map

-}

import Elm3d.Color exposing (Color)
import Elm3d.Context
import Elm3d.Input.Event
import Elm3d.Internals.Node as Node
import Elm3d.Texture
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector3 exposing (Vector3)


{-| A node is something that can be rendered by an Elm3d program
-}
type alias Node msg =
    Node.Node msg


{-| Create a cube with the given size and color.

    import Elm3d.Color

    redCube : Node msg
    redCube =
        cube
            { size = 1
            , color = Elm3d.Color.red
            }

-}
cube : { size : Float, color : Color } -> Node msg
cube props =
    Node.cube
        { size = props.size
        , texture = Elm3d.Texture.color props.color
        }


{-| Create a rectangular block with the specified side lengths and color.

    import Elm3d.Color
    import Elm3d.Vector3

    blueBlock : Node msg
    blueBlock =
        block
            { size = Elm3d.Vector3.size 1 2 4
            , color = Elm3d.Color.blue
            }

-}
block : { size : Vector3, color : Color } -> Node msg
block props =
    Node.block
        { size = props.size
        , texture = Elm3d.Texture.color props.color
        }


{-| Create a 3D model from an OBJ file.

    tavern : Node msg
    tavern =
        obj { url = "/assets/tavern.obj" }

When used with an `Elm3d.Program`, this will _automatically
fetch_ any required `.mtl` and `.png` textures via HTTP requests.

This is an **incomplete implementation** of [the OBJ spec](https://paulbourke.net/dataformats/obj/).
It doesn't cover all aspects of rendering materials. It was initially designed to render the assets from this [KayKit 3D asset pack](https://kaylousberg.itch.io/kaykit-medieval-hexagon).

-}
obj : { url : String } -> Node msg
obj props =
    Node.obj props


{-| Create a group of nodes, so you can organize transform them all at once.

    buildings : Node msg
    buildings =
        group
            [ obj { url = "/assets/tavern.obj" }
            , obj { url = "/assets/market.obj" }
            ]

-}
group : List (Node msg) -> Node msg
group nodes =
    Node.group nodes


{-| Set this node's position using a 3D vector.

    import Elm3d.Vector3 exposing (Vector3)

    position : Vector3
    position =
        Elm3d.Vector3.new 5 2 -1

    myCube : Node msg
    myCube =
        cube { size = 1, color = red }
            |> withPosition position

-}
withPosition : Vector3 -> Node msg -> Node msg
withPosition props node =
    Node.withPosition props node


{-| Set this node's position along the **x-axis**.

    cube { size = 1, color = red }
        |> withPositionX 5

-}
withPositionX : Float -> Node msg -> Node msg
withPositionX props node =
    Node.withPositionX props node


{-| Set this node's position along the **y-axis**.

    cube { size = 1, color = red }
        |> withPositionY 5

-}
withPositionY : Float -> Node msg -> Node msg
withPositionY props node =
    Node.withPositionY props node


{-| Set this node's position along the **z-axis**.

    cube { size = 1, color = red }
        |> withPositionZ 5

-}
withPositionZ : Float -> Node msg -> Node msg
withPositionZ props node =
    Node.withPositionZ props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withRotation 2

-}
withRotation : Vector3 -> Node msg -> Node msg
withRotation props node =
    Node.withRotation props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withRotationX 2

-}
withRotationX : Float -> Node msg -> Node msg
withRotationX props node =
    Node.withRotationX props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withRotationY 2

-}
withRotationY : Float -> Node msg -> Node msg
withRotationY props node =
    Node.withRotationY props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withRotationZ 2

-}
withRotationZ : Float -> Node msg -> Node msg
withRotationZ props node =
    Node.withRotationZ props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withScale 2

-}
withScale : Vector3 -> Node msg -> Node msg
withScale props node =
    Node.withScale props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withScaleX 2

-}
withScaleX : Float -> Node msg -> Node msg
withScaleX props node =
    Node.withScaleX props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withScaleY 2

-}
withScaleY : Float -> Node msg -> Node msg
withScaleY props node =
    Node.withScaleY props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withScaleZ 2

-}
withScaleZ : Float -> Node msg -> Node msg
withScaleZ props node =
    Node.withScaleZ props node



-- HANDLING EVENTS


{-| Provide a message to call every frame.

    import Elm3d.Context exposing (Context)

    type Msg
        = TavernOnFrame Context

    myCube : Node Msg
    myCube =
        cube { size = 1, color = red }
            |> withOnFrame TavernOnFrame

-}
withOnFrame : (Elm3d.Context.Context -> msg) -> Node msg -> Node msg
withOnFrame props node =
    Node.withOnFrame props node


{-| Provide a message to call for any input event.

    import Elm3d.Input.Event exposing (Event)

    type Msg
        = TavernOnInput Event

    myCube : Node Msg
    myCube =
        cube { size = 1, color = red }
            |> withOnInput TavernOnInput

-}
withOnInput : (Elm3d.Input.Event.Event -> msg) -> Node msg -> Node msg
withOnInput props node =
    Node.withOnInput props node


{-| Convert a node with one `msg` type into another. This is commonly needed when nesting
the Elm Architecture with a page or component:

    import Pages.Dashboard

    type Msg
        = Dashboard Pages.Dashboard.Msg

    node : Node Pages.Dashboard.Msg
    node =
        ...

    newNode : Node Msg
    newNode =
        map Dashboard node

-}
map : (msg1 -> msg2) -> Node msg1 -> Node msg2
map fn node =
    Node.map fn node
