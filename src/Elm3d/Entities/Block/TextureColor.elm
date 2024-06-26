module Elm3d.Entities.Block.TextureColor exposing (Uniforms, toEntity)

import Elm3d.Color exposing (Color)
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Vector3 exposing (Vector3)
import WebGL
import WebGL.Settings
import WebGL.Settings.Blend
import WebGL.Settings.DepthTest


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        uniform vec3 scale;
        uniform mat4 camera;
        uniform mat4 modelView;
        varying vec3 v_position;
        varying mat4 v_modelView;
    
        void main () {
            gl_Position = camera * modelView * vec4(scale * position, 1.0);
            v_position = position;
            v_modelView = modelView;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision highp float;
        uniform vec4 color;
        uniform vec3 lightDirection;
        varying vec3 v_position;
        varying mat4 v_modelView;

        void main () {
            float light = 1.0;
            if (length(lightDirection) > 0.0) {
                vec3 normal = vec3(
                    v_position.x >= 0.5 ? 1.0 : v_position.x <= -0.5 ? -1.0 : 0.0,
                    v_position.y >= 0.5 ? 1.0 : v_position.y <= -0.5 ? -1.0 : 0.0,
                    v_position.z >= 0.5 ? 1.0 : v_position.z <= -0.5 ? -1.0 : 0.0
                );

                normal = mat3(v_modelView) * normal;
                light = dot(normalize(normal), normalize(lightDirection));
            }

            gl_FragColor = color;
            gl_FragColor.rgb *= clamp(light, 0.0, 1.0);
        }
    |]


type alias Attributes =
    { position : Vector3
    }


type alias Uniforms =
    { scale : Vector3
    , camera : Matrix4
    , modelView : Matrix4
    , color : Color
    , lightDirection : Vector3
    }


type alias Varyings =
    { v_position : Vector3
    , v_modelView : Matrix4
    }


toEntity : Uniforms -> WebGL.Entity
toEntity uniforms =
    WebGL.entityWith
        [ WebGL.Settings.DepthTest.default
        , WebGL.Settings.cullFace WebGL.Settings.back
        , WebGL.Settings.Blend.add
            WebGL.Settings.Blend.one
            WebGL.Settings.Blend.oneMinusSrcAlpha
        ]
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
