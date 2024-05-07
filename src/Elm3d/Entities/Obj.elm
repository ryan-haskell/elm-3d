module Elm3d.Entities.Obj exposing (Uniforms, toEntity)

import Elm3d.Color exposing (Color)
import Elm3d.File.Obj
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Vector3 exposing (Vector3)
import List.Extra
import Math.Vector2
import WebGL
import WebGL.Settings.DepthTest
import WebGL.Texture exposing (Texture)


type alias Attributes =
    { position : Vector3
    , normal : Vector3
    , uv : Math.Vector2.Vec2
    }


type alias Uniforms =
    { camera : Matrix4
    , modelView : Matrix4
    , lightDirection : Vector3
    , texture : Texture
    }


type alias Varyings =
    { v_normal : Vector3
    , v_uv : Math.Vector2.Vec2
    }


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec2 uv;
        
        uniform mat4 camera;
        uniform mat4 modelView;

        varying vec3 v_normal;
        varying vec2 v_uv;
    
        void main () {
            gl_Position = camera * modelView * vec4(position, 1.0);
            v_normal = mat3(modelView) * normal;
            v_uv = uv;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 lightDirection;
        uniform sampler2D texture;
        varying vec3 v_normal;
        varying highp vec2 v_uv;

        void main () {
            vec3 normal = normalize(v_normal);
            float light;
            if (length(lightDirection) == 0.0) {
                light = 1.0;
            } else {
                light = dot(normal, normalize(lightDirection));
            }

            gl_FragColor = texture2D(texture, v_uv);
            gl_FragColor.rgb *= light;
        }
    |]


toEntity : Elm3d.File.Obj.Data -> Uniforms -> WebGL.Entity
toEntity data uniforms =
    WebGL.entityWith [ WebGL.Settings.DepthTest.default ]
        vertexShader
        fragmentShader
        (Elm3d.File.Obj.toMesh data)
        uniforms
