# **@ryan-haskell/elm-3d**
A package for making 3D experiences in Elm

## **Local installation**

```
elm install ryan-haskell/elm-3d
```

## **Quick example**

![Demo of two 3D buildings spinning on a white background](https://github.com/ryan-haskell/elm-3d/blob/main/example/webgl_obj_mtl.gif?raw=true)


```elm
import Elm3d.Camera exposing (Camera)
import Elm3d.Context exposing (Context)
import Elm3d.Node exposing (Node)
import Elm3d.Program exposing (Program, View)
import Elm3d.Viewport


main : Program () Model Msg
main =
    Elm3d.Program.sandbox
        { init = init
        , update = update
        , view = view
        , onReady = AssetsLoaded
        }



-- MODEL


type alias Model =
    { angle : Float
    }


init : Model
init =
    { angle = 0
    }



-- UPDATE


type Msg
    = AssetsLoaded
    | Spin Context


update : Msg -> Model -> Model
update msg model =
    case msg of
        AssetsLoaded ->
            model

        Spin ctx ->
            { model | angle = model.angle + ctx.dt }



-- VIEW


view : Model -> View Msg
view model =
    { viewport = Elm3d.Viewport.fullscreen
    , background = Elm3d.Color.white
    , camera =
        Elm3d.Camera.perspective
            { fov = 60
            , range = ( 1, 100 )
            }
            |> Elm3d.Camera.withPositionZ 5
    , nodes =
        [ buildings model
        ]
    }


buildings : Model -> Node Msg
buildings model =
    Elm3d.Node.group
        [ tavern
        , church
        ]
        |> Elm3d.Node.withOnFrame Spin
        |> Elm3d.Node.withRotationY model.angle


tavern : Node Msg
tavern =
    Elm3d.Node.obj
        { url = "/assets/buildings/tavern.obj"
        }
        |> Elm3d.Node.withPositionX 1


church : Node Msg
church =
    Elm3d.Node.obj
        { url = "/assets/buildings/church.obj"
        }
        |> Elm3d.Node.withPositionX -1
```

## **Disclaimer**

This package was originally created from code for use on my website. Rather than keeping that code private, I extracted out and documented the parts that I thought would be helpful for others creating 3D things in Elm.

I encourage you to use this package to create your own 3D programs. Please feel welcome to modify my open source code if there are any features you would like to add.

Although my code is free to take, and the package is free to use, it __does not come with any promise for future maintenance__. This is a tiny gift from me to the Elm community, and there are many other cool Elm things I want to spend my time on after sharing it.

I would love to see support for _lighting_, _GLTF imports_, _physics_, _raycasting_, and much more. My initial goal for this package was making it the easiest way for folks to quickly create 3D games and experiences with Elm.

If you are personally interested in driving any fraction of that vision forward, I'm happy to chat about those designs and features. Please reach out to me on the [Elm Slack](https://elm-lang.org/community/slack) (`@ryan`) and we can make it happen!