import Browser
import Browser.Events as Events
import Html exposing (Html)
import Html.Attributes as Html
import Json.Decode as D
import List
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL

import NonEmptyList

type alias Model =
    { mousePosition : Vec2 }

type Msg = MouseMoved Vec2Rec

canvasWidth : Float
canvasWidth = 650

canvasHeight : Float
canvasHeight = 750

main : Program () Model Msg
main = Browser.element
    { init = \_ -> (Model (vec2 0.2 0.2), Cmd.none)
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.map MouseMoved <|
        Events.onMouseMove <|
            D.map2 Vec2Rec
                (D.field "clientX" D.float)
                (D.field "clientY" D.float)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MouseMoved pos ->
            ( { model | mousePosition =
                    vec2
                        (pos.x / canvasWidth * 2 - 1)
                        (pos.y / -canvasHeight * 2 + 1)
              }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    let
        rays = lines { color = red } (rayCastLevel model.mousePosition)
        --rays = lines { color = red }
        --           [ rayCast { pos = Vec2Rec 0 0, dir = Vec2.toRecord model.mousePosition } ]
        levelShapes = List.map (polygon { color = black }) level
    in
        WebGL.toHtml [ Html.width 650, Html.height 750 ] (rays :: levelShapes)

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
        t2 = (ray.dir.x * (seg.pos.y - ray.pos.y) +
              ray.dir.y * (ray.pos.x - seg.pos.x)) /
             (seg.dir.x*ray.dir.y - seg.dir.y*ray.dir.x)
        t1 = (seg.pos.x + seg.dir.x*t2 - ray.pos.x) / ray.dir.x
        dirVec = Vec2.fromRecord ray.dir
    in
        if t1 > 0 && -0.02 < t2 && t2 < 1.02 then
            Just
                { t1 = t1
                , point =
                    Vec2.add
                        (Vec2.fromRecord ray.pos)
                        (Vec2.scale t1 dirVec)
                }
        else
            Nothing

rayCast : Ray -> (Vec2, Vec2)
rayCast ray =
    let
        hits = List.filterMap (findIntersection ray) levelSegments
    in
        case NonEmptyList.fromList hits of
            Nothing -> (Vec2.fromRecord ray.pos, Vec2.fromRecord ray.dir)
            Just l -> (Vec2.fromRecord ray.pos, (NonEmptyList.findMin .t1 l).point)

rayCastLevel : Vec2 -> List (Vec2, Vec2)
rayCastLevel startPoint =
    let rays = List.map (rayFromPoints startPoint) (List.concat level)
    in List.map rayCast rays

headAndLast : List a -> Maybe (a, a)
headAndLast list =
    let
        findLast l =
            case l of
                [] -> Nothing
                (x::[]) -> Just x
                (_::xs) -> findLast xs
    in
        Maybe.map2
            (\x y -> (x, y))
            (List.head list)
            (Maybe.andThen findLast <| List.tail list)

combineConsecutive : (a -> a -> b) -> List a -> List b
combineConsecutive combFun list =
    let
        internal l =
            case l of
                [] -> []
                (_::[]) -> []
                (x::y::xs) -> combFun x y :: internal (y::xs)
    in
        case headAndLast list of
            Nothing -> []
            Just (h, l) -> combFun h l :: internal list

rayFromPoints : Vec2 -> Vec2 -> Ray
rayFromPoints start end =
    { pos = Vec2.toRecord start
    , dir = Vec2.toRecord (Vec2.sub end start)
    }

levelSegments : List Ray
levelSegments =
    List.concatMap (combineConsecutive rayFromPoints) level

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
    -- Borders
    , [ vec2 -1 1, vec2 1 1, vec2 1 -1, vec2 -1 -1]
    ]

type alias Color = Vec3

black : Color
black = vec3 0 0 0

red : Color
red = vec3 1 0 0

type alias Uniforms =
    { color : Color
    }

polygon : Uniforms -> List Vec2 -> WebGL.Entity
polygon uniforms vertices =
    let
        mesh = WebGL.lineLoop (List.map LineAttributes vertices)
    in
        WebGL.entity vertexShader fragmentShader mesh uniforms

concatTupleList : List (a, a) -> List a
concatTupleList = List.concatMap (\(x1, x2) -> [x1, x2])

lines : Uniforms -> List (Vec2, Vec2) -> WebGL.Entity
lines uniforms vertices =
    let
        mesh = WebGL.lines (List.map (Tuple.mapBoth LineAttributes LineAttributes) vertices)
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

precision mediump float;

uniform vec3 color;

void main () {
  gl_FragColor = vec4(color, 1.0);
}

|]
