module Elm3d.Entities.Obj exposing
    ( UniformsT0, toEntityT0
    , UniformsT1, toEntityT1
    , findTextures
    )

{-|

@docs UniformsT0, toEntityT0
@docs UniformsT1, toEntityT1

(TODO: I'll need more of these to support more texture files!)

-}

import Dict
import Elm3d.Asset
import Elm3d.Color exposing (Color)
import Elm3d.File.Obj
import Elm3d.Matrix4 exposing (Matrix4)
import Elm3d.Vector3 exposing (Vector3)
import List.Extra
import Math.Vector2
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest
import WebGL.Texture exposing (Texture)


findTextures :
    Elm3d.File.Obj.Data
    -> Elm3d.Asset.Model
    -> Maybe (List Texture)
findTextures obj assets =
    let
        textureUrls : List String
        textureUrls =
            obj.info.mtl
                |> List.filterMap (\url -> Elm3d.Asset.findMtl url assets)
                |> List.map .materials
                |> List.concatMap Dict.values
                |> List.filterMap .map_Kd

        dependencies : List String
        dependencies =
            obj.info.mtl ++ textureUrls

        isLoading : String -> Bool
        isLoading url =
            Elm3d.Asset.isLoading url assets
    in
    if List.any isLoading dependencies then
        Nothing

    else
        textureUrls
            |> List.filterMap (\textureUrl -> Elm3d.Asset.findPng textureUrl assets)
            |> Just



-- RENDERING


type alias Attributes =
    Elm3d.File.Obj.Attributes


type alias Varyings =
    { v_normal : Vector3
    , v_uv : Math.Vector2.Vec2
    , v_kd : Vector3
    }



-- 0 TEXTURES


type alias UniformsT0 =
    { camera : Matrix4
    , modelView : Matrix4
    , lightDirection : Vector3
    }


vertexShaderT0 : WebGL.Shader Attributes UniformsT0 Varyings
vertexShaderT0 =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec2 uv;
        attribute vec3 kd;
        
        uniform mat4 camera;
        uniform mat4 modelView;

        varying vec3 v_normal;
        varying vec2 v_uv;
        varying vec3 v_kd;
    
        void main () {
            gl_Position = camera * modelView * vec4(position, 1.0);
            v_normal = mat3(modelView) * normal;
            v_uv = uv;
            v_kd = kd;
        }
    |]


fragmentShaderT0 : WebGL.Shader {} UniformsT0 Varyings
fragmentShaderT0 =
    [glsl|
        precision mediump float;

        uniform vec3 lightDirection;

        varying vec3 v_normal;
        varying highp vec2 v_uv;
        varying vec3 v_kd;

        void main () {
            vec3 normal = normalize(v_normal);
            float light;
            if (length(lightDirection) == 0.0) {
                light = 1.0;
            } else {
                light = dot(normal, normalize(lightDirection));
            }

            gl_FragColor = vec4(v_kd, 1.0);
            gl_FragColor.rgb *= clamp(light, 0.0, 1.0);
        }
    |]


toEntityT0 : WebGL.Mesh Attributes -> UniformsT0 -> WebGL.Entity
toEntityT0 mesh uniforms =
    WebGL.entityWith settings
        vertexShaderT0
        fragmentShaderT0
        mesh
        uniforms



-- HAS ONE TEXTURE


type alias UniformsT1 =
    { camera : Matrix4
    , modelView : Matrix4
    , lightDirection : Vector3
    , texture : Texture
    }


vertexShaderT1 : WebGL.Shader Attributes UniformsT1 Varyings
vertexShaderT1 =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec2 uv;
        attribute vec3 kd;
        
        uniform mat4 camera;
        uniform mat4 modelView;

        varying vec3 v_normal;
        varying vec2 v_uv;
        varying vec3 v_kd;
    
        void main () {
            gl_Position = camera * modelView * vec4(position, 1.0);
            v_normal = mat3(modelView) * normal;
            v_uv = uv;
            v_kd = kd;
        }
    |]


fragmentShaderT1 : WebGL.Shader {} UniformsT1 Varyings
fragmentShaderT1 =
    [glsl|
        precision mediump float;

        uniform vec3 lightDirection;
        uniform sampler2D texture;

        varying vec3 v_normal;
        varying highp vec2 v_uv;
        varying vec3 v_kd;

        void main () {
            vec3 normal = normalize(v_normal);
            float light;
            if (length(lightDirection) == 0.0) {
                light = 1.0;
            } else {
                light = dot(normal, normalize(lightDirection));
            }

            gl_FragColor = texture2D(texture, v_uv);
            gl_FragColor.rgb *= clamp(light, 0.0, 1.0);
        }
    |]


toEntityT1 : WebGL.Mesh Attributes -> UniformsT1 -> WebGL.Entity
toEntityT1 mesh uniforms =
    WebGL.entityWith settings
        vertexShaderT1
        fragmentShaderT1
        mesh
        uniforms


settings : List WebGL.Settings.Setting
settings =
    [ WebGL.Settings.DepthTest.default
    , WebGL.Settings.cullFace WebGL.Settings.back
    ]
