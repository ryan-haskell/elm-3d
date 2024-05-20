module Elm3d.Node.Public exposing
    ( Node
    , cube, block
    , obj
    , withPosition
    , withPositionX, withPositionY, withPositionZ
    , withRotation
    , withRotationX, withRotationY, withRotationZ
    , withScale
    , withScaleX, withScaleY, withScaleZ
    , withOnFrame, withOnInput
    )

{-| This module allows you to define 3D objects, lights, cameras and more for use in an Elm3d program.


# **Creating nodes**

@docs Node
@docs cube, block
@docs obj


# **Moving, rotating, scaling**

@docs withPosition
@docs withPositionX, withPositionY, withPositionZ

@docs withRotation
@docs withRotationX, withRotationY, withRotationZ

@docs withScale
@docs withScaleX, withScaleY, withScaleZ


# **Handling events**

@docs withOnFrame, withOnInput

-}

import Elm3d.Color exposing (Color)
import Elm3d.Context
import Elm3d.Input.Event
import Elm3d.Node
import Elm3d.Texture
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector3 exposing (Vector3)


{-| A node is something that can be rendered by an Elm3d program
-}
type alias Node msg =
    Elm3d.Node.Node msg


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
    Elm3d.Node.cube
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
    Elm3d.Node.block
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
    Elm3d.Node.obj props


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
    Elm3d.Node.withPosition props node


{-| Set this node's position along the **x-axis**.

    cube { size = 1, color = red }
        |> withPositionX 5

-}
withPositionX : Float -> Node msg -> Node msg
withPositionX props node =
    Elm3d.Node.withPositionX props node


{-| Set this node's position along the **y-axis**.

    cube { size = 1, color = red }
        |> withPositionY 5

-}
withPositionY : Float -> Node msg -> Node msg
withPositionY props node =
    Elm3d.Node.withPositionY props node


{-| Set this node's position along the **z-axis**.

    cube { size = 1, color = red }
        |> withPositionZ 5

-}
withPositionZ : Float -> Node msg -> Node msg
withPositionZ props node =
    Elm3d.Node.withPositionZ props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withRotation 2

-}
withRotation : Vector3 -> Node msg -> Node msg
withRotation props node =
    Elm3d.Node.withRotation props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withRotationX 2

-}
withRotationX : Float -> Node msg -> Node msg
withRotationX props node =
    Elm3d.Node.withRotationX props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withRotationY 2

-}
withRotationY : Float -> Node msg -> Node msg
withRotationY props node =
    Elm3d.Node.withRotationY props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withRotationZ 2

-}
withRotationZ : Float -> Node msg -> Node msg
withRotationZ props node =
    Elm3d.Node.withRotationZ props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withScale 2

-}
withScale : Vector3 -> Node msg -> Node msg
withScale props node =
    Elm3d.Node.withScale props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withScaleX 2

-}
withScaleX : Float -> Node msg -> Node msg
withScaleX props node =
    Elm3d.Node.withScaleX props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withScaleY 2

-}
withScaleY : Float -> Node msg -> Node msg
withScaleY props node =
    Elm3d.Node.withScaleY props node


{-|

    myCube =
        cube { size = 1, color = red }
            |> withScaleZ 2

-}
withScaleZ : Float -> Node msg -> Node msg
withScaleZ props node =
    Elm3d.Node.withScaleZ props node


{-| Provide a message to call every frame.

    import Elm3d.Context exposing (Context)

    type Msg
        = TavernOnFrame Context

    myCube : Node msg
    myCube =
        cube { size = 1, color = red }
            |> withOnFrame TavernOnFrame

-}
withOnFrame : (Elm3d.Context.Context -> msg) -> Node msg -> Node msg
withOnFrame props node =
    Elm3d.Node.withOnFrame props node


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
    Elm3d.Node.withOnInput props node
