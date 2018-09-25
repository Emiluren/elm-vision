import Browser
import Html exposing (Html)
import Html.Attributes as Html
import List
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL

type alias Model = ()

main : Program () Model msg
main = Browser.sandbox { init = (), update = \_ x -> x, view = view }

view : Model -> Html msg
view model =
    WebGL.toHtml [ Html.width 650, Html.height 750 ]
        (List.map (polygon { color = black }) level)

type alias LineAttributes =
    { position : Vec2
    }

type alias Vec2Rec = { x : Float, y : Float }

type alias Ray =
    { pos : Vec2Rec
    , dir : Vec2Rec
    }

type alias RayHit =
    { t1 : Float
    , point : Vec2
    }

findIntersection : Ray -> Ray -> Maybe RayHit
findIntersection ray seg =
    let
        t2 = (ray.pos.x*(seg.pos.y-ray.pos.y) +
              ray.dir.y*(ray.pos.x-seg.pos.x)) /
             (seg.dir.x*ray.dir.y - seg.dir.y*ray.dir.x)
        t1 = (seg.pos.x + seg.dir.x*t2 - ray.pos.x) / ray.dir.x
        dirVec = Vec2.fromRecord ray.dir
    in
        if t1 > 0 && 0 < t2 && t2 < 1 then
            Just
                { t1 = t1
                , point =
                    Vec2.add
                        (Vec2.fromRecord ray.pos)
                        (Vec2.scale t1 dirVec)
                }
        else
            Nothing

level : List (List Vec2)
level =
    -- Left
    [ [ vec2 -0.2 -0.8, vec2 -0.4 -0.7, vec2 -0.6 -0.2 , vec2 0 -0.3 ]
    , [ vec2 -0.8 0, vec2 -0.4 0.2, vec2 -0.5 -0.1 ]
    , [ vec2 -0.7 0.55, vec2 -0.6 0.6, vec2 -0.5 0.5]
    -- Right
    , [ vec2 0.3 0.8, vec2 0.6 0.2 , vec2 0.2 0.3 ]
    , [ vec2 0.8 0, vec2 0.4 -0.2, vec2 0.5 0.1 ]
    , [ vec2 0.7 -0.55, vec2 0.6 -0.6, vec2 0.5 -0.5, vec2 0.55 -0.4]
    ]

type alias Color = Vec3

black : Color
black = vec3 0 0 0

type alias Uniforms =
    { color : Color
    }

polygon : Uniforms -> List Vec2 -> WebGL.Entity
polygon uniforms vertices =
    let
        mesh = WebGL.lineLoop (List.map LineAttributes vertices)
    in
        WebGL.entity vertexShader fragmentShader mesh uniforms

vertexShader : WebGL.Shader LineAttributes u {}
vertexShader = [glsl|

attribute vec2 position;

void main () {
  gl_Position = vec4(position, 0.0, 1.0);
}

|]

fragmentShader : WebGL.Shader {} Uniforms {}
fragmentShader = [glsl|

uniform vec3 color;

void main () {
  gl_FragColor = vec4(color, 1.0);
}

|]
