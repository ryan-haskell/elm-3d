module Elm3d.Entities.Cube.TextureColor exposing (Uniforms, toEntity)

import Elm3d.Color exposing (Color)
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Vector3 exposing (Vector3)
import WebGL
import WebGL.Settings.DepthTest


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        uniform float scale;
        uniform mat4 camera;
        uniform mat4 modelView;
    
        void main () {
            gl_Position = camera * modelView * vec4(scale * position, 1.0);
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec4 color;

        void main () {
            gl_FragColor = color;
        }
    |]


type alias Attributes =
    { position : Vector3
    }


type alias Uniforms =
    { scale : Float
    , camera : Matrix4
    , modelView : Matrix4
    , color : Color
    }


type alias Varyings =
    {}


toEntity : Uniforms -> WebGL.Entity
toEntity uniforms =
    WebGL.entityWith [ WebGL.Settings.DepthTest.default ]
        vertexShader
        fragmentShader
        mesh
        uniforms


mesh : WebGL.Mesh Attributes
mesh =
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
