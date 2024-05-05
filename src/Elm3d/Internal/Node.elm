module Elm3d.Internal.Node exposing
    ( Node
    , cube
    , toEntity
    , withPosition
    , withTextureColor
    )

import Elm3d.Camera exposing (Camera)
import Elm3d.Color exposing (Color)
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Transform3d exposing (Transform3d)
import Elm3d.Vector3 exposing (Vector3)
import Elm3d.Vector4 exposing (Vector4)
import WebGL
import WebGL.Settings.DepthTest


type Node
    = Node Internals


type Mesh
    = Cube { size : Float }


type alias Internals =
    { mesh : Mesh
    , transform : Transform3d
    , texture : Texture
    }


type Texture
    = TextureColor Vector4


cube : { size : Float } -> Node
cube props =
    Node
        { mesh = Cube props
        , transform = Elm3d.Transform3d.none
        , texture = TextureColor Elm3d.Vector4.one
        }


withPosition : Vector3 -> Node -> Node
withPosition props (Node node) =
    Node { node | transform = Elm3d.Transform3d.withPosition props node.transform }


withTextureColor : Color -> Node -> Node
withTextureColor props (Node node) =
    Node { node | texture = TextureColor props }


toEntity : { camera : Matrix4 } -> Node -> WebGL.Entity
toEntity { camera } ((Node { texture, transform, mesh }) as node) =
    case ( texture, mesh ) of
        ( TextureColor color, Cube { size } ) ->
            WebGL.entityWith [ WebGL.Settings.DepthTest.default ]
                vertexShader
                fragmentShader
                (toMesh node)
                { scale = size
                , color = color
                , modelView = Elm3d.Transform3d.toMatrix4 transform
                , camera = camera
                }


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        uniform float scale;
        uniform mat4 camera;
        uniform mat4 modelView;
        varying vec3 v_normal;
    
        void main () {
            gl_Position = camera * modelView * vec4(scale * position, 1.0);
            v_normal = position;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec4 color;
        varying vec3 v_normal;

        void main () {
            gl_FragColor = color;
        }
    |]


type alias Attributes =
    { position : Vector3
    }


type alias Uniforms =
    { scale : Float
    , camera : Elm3d.Matrix4.Matrix4
    , modelView : Elm3d.Matrix4.Matrix4
    , color : Elm3d.Color.Color
    }


type alias Varyings =
    { v_normal : Vector3
    }


toMesh : Node -> WebGL.Mesh Attributes
toMesh (Node node) =
    case node.mesh of
        Cube { size } ->
            let
                vf =
                    Elm3d.Vector3.new 0.5 0.5 0.5

                vg =
                    Elm3d.Vector3.new 0.5 0.5 -0.5

                vb =
                    Elm3d.Vector3.new 0.5 -0.5 0.5

                vd =
                    Elm3d.Vector3.new 0.5 -0.5 -0.5

                ve =
                    Elm3d.Vector3.new -0.5 0.5 0.5

                vh =
                    Elm3d.Vector3.new -0.5 0.5 -0.5

                va =
                    Elm3d.Vector3.new -0.5 -0.5 0.5

                vc =
                    Elm3d.Vector3.new -0.5 -0.5 -0.5
            in
            [ vd, vc, vg, vh, ve, vc, va, vd, vb, vg, vf, ve, vb, va ]
                |> List.map Attributes
                |> WebGL.triangleStrip
