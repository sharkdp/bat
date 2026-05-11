-- elm install elm-explorations/linear-algebra
-- elm install elm-explorations/webgl


import Browser
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  Float


init : () -> (Model, Cmd Msg)
init () =
  ( 0, Cmd.none )



-- UPDATE


type Msg
  = TimeDelta Float


update : Msg -> Model -> (Model, Cmd Msg)
update msg currentTime =
  case msg of
    TimeDelta delta ->
      ( delta + currentTime, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta



-- VIEW


view : Model -> Html msg
view t =
  WebGL.toHtml
    [ width 400, height 400, style "display" "block"
    ]
    [ WebGL.entity vertexShader fragmentShader mesh { perspective = perspective (t / 1000) }
    ]


perspective : Float -> Mat4
perspective t =
  Mat4.mul
    (Mat4.makePerspective 45 1 0.01 100)
    (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))



-- MESH


type alias Vertex =
  { position : Vec3
  , color : Vec3
  }


mesh : WebGL.Mesh Vertex
mesh =
  WebGL.triangles
    [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
      , Vertex (vec3 1 1 0) (vec3 0 1 0)
      , Vertex (vec3 1 -1 0) (vec3 0 0 1)
      )
    ]



-- SHADERS


type alias Uniforms =
  { perspective : Mat4
  }


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
