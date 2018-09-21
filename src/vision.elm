import Browser
import Html exposing (Html)
import WebGL
import Math.Vector3 exposing (Vec3, vec3)

main = Browser.sandbox { init = (), update = \x -> x, view = view }

view model =
    WebGL.toHtml []
        [ WebGL.entity vertexShader fragmentShader polygon {} ]

type alias Attributes =
    { position : Vec3
    }

polygon =
    let
        vertices =
            [ vec3 0 0 0
            , vec3 0 1 0
            , vec3 1 0 0
            ]
    in
        WebGL.lineLoop (List.map Attributes vertices)

vertexShader : WebGL.Shader Attributes {} {}
vertexShader = [glsl|

attribute vec3 position;

void main () {
  gl_Position = vec4(position, 1.0);
}

|]

fragmentShader : WebGL.Shader {} {} {}
fragmentShader = [glsl|

void main () {
  gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
}

|]
