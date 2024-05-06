module Elm3d.Entities.Obj exposing (Uniforms, toEntity)

import Elm3d.Color exposing (Color)
import Elm3d.File.Obj
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Vector3 exposing (Vector3)
import List.Extra
import WebGL
import WebGL.Settings.DepthTest


type alias Attributes =
    { position : Vector3
    , normal : Vector3
    }


type alias Uniforms =
    { camera : Matrix4
    , modelView : Matrix4
    , lightDirection : Vector3
    }


type alias Varyings =
    { v_normal : Vector3
    }


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        
        uniform mat4 camera;
        uniform mat4 modelView;

        varying vec3 v_normal;
    
        void main () {
            gl_Position = camera * modelView * vec4(position, 1.0);
            v_normal = mat3(modelView) * normal;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 lightDirection;
        varying vec3 v_normal;

        void main () {
            vec3 normal = normalize(v_normal);
            float light = dot(normal, normalize(lightDirection));

            gl_FragColor = vec4(1.0, 0.8, 0.5, 1.0);
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
