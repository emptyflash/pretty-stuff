module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Json
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Task
import WebGL exposing (Mesh, Shader)
import WebGL.Settings


type alias Point =
    { x : Float
    , y : Float
    }


half : Point -> Point
half size =
    { x = size.x / 2
    , y = size.y / 2
    }


type Msg
    = UpdateTime Float
    | WindowResize Int Int
    | MouseMoved Point
    | KeyUp String
    | KeyDown String


type alias Circle =
    { point : Vec2
    , color : Vec3
    , pointSize : Float
    }


type alias Model =
    { time : Float
    , circles : List Circle
    , size : Point
    , mouse : Point
    , factor : Float
    , angle : Float
    , w : Bool
    , a : Bool
    , s : Bool
    , d : Bool
    }


view : Model -> Html Msg
view model =
    WebGL.toHtmlWith
        [ WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0.0 0.0 0.0 1.0
        ]
        [ model.size.x |> floor |> width
        , model.size.y |> floor |> height
        ]
        [ WebGL.entityWith
            [ WebGL.Settings.sampleAlphaToCoverage ]
            vertexShader
            fragmentShader
            (WebGL.points <| model.circles)
            { size = vec2 model.size.x model.size.y
            , perspective = perspective model.size
            }
        ]


perspective : Point -> Mat4
perspective size =
    Mat4.makeOrtho2D
        (-1.0 * size.x / size.y)
        (1.0 * size.x / size.y)
        -1.0
        1.0


type alias Uniforms =
    { size : Vec2
    , perspective : Mat4
    }


vertexShader : Shader Circle Uniforms { outColor : Vec4 }
vertexShader =
    [glsl|
        attribute vec2 point;
        attribute vec3 color;
        attribute float pointSize;
        varying vec4 outColor;
        uniform mat4 perspective;

        void main () {
            gl_PointSize = pointSize;
            vec4 pos = vec4(point, 0.0, 1.0);
            gl_Position = perspective * pos;
            outColor = vec4(color, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms { outColor : Vec4 }
fragmentShader =
    [glsl|
        precision highp float;
        varying vec4 outColor;

        float circle(in vec2 st, in float radius) {
            vec2 dist = st - vec2(0.5);
            return 1.0 - smoothstep(radius - (radius * 0.01), radius + (radius * 0.01), dot(dist, dist) * 4.0);
        }

        vec3 hsb2rgb( in vec3 c ){
            vec3 rgb = clamp(abs(mod(c.x*6.0+vec3(0.0,4.0,2.0), 6.0)-3.0)-1.0, 0.0, 1.0 );
            rgb = rgb*rgb*(3.0-2.0*rgb);
            return c.z * mix(vec3(1.0), rgb, c.y);
        }

        void main() {
            float r = 0.0, delta = 0.0, alpha = 1.0;
            vec2 cxy = 2.0 * gl_PointCoord - 1.0;
            r = dot(cxy, cxy);
            delta = r * 0.01;
            alpha = 1.0 - smoothstep(1.0 - delta, 1.0 + delta,  8.0 * r);
            gl_FragColor = vec4(hsb2rgb(outColor.xyz), alpha);
        }
    |]


fmod : Float -> Float -> Float
fmod modulus x =
    x - modulus * toFloat (floor (x / modulus))


maxDepth : Int
maxDepth =
    1000


mouseToWebGL : Point -> Point -> Vec2
mouseToWebGL size mouse =
    let
        result =
            Mat4.transform
                (Maybe.withDefault
                    Mat4.identity
                    (Mat4.inverse (perspective size))
                )
                (vec3
                    (mouse.x / size.x * 2 - 1)
                    -(mouse.y / size.y * 2 - 1)
                    1
                )
    in
    vec2 (Vec3.getX result) (Vec3.getY result)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTime time ->
            let
                angle =
                    if model.a then
                        model.angle + 0.01

                    else if model.d then
                        model.angle - 0.01

                    else
                        model.angle

                factor =
                    if model.w then
                        model.factor + 0.01

                    else if model.s then
                        model.factor - 0.01

                    else
                        model.factor

                {--
                globalTransform =
                    [ Translate
                        (-center.x * (model.factor - 1))
                        (-center.y * (model.factor - 1))
                    , Scale model.factor model.factor
                    , Rotate model.angle center.x center.y
                    ]
                --}
                transform =
                    Mat4.makeRotate angle (vec3 0 0 1)
                        |> Mat4.scale (vec3 factor factor 1)

                --Mat4.makeScale3 factor factor 1.0
                --|> Mat4.rotate angle (vec3 0 0 0)
                circles =
                    { point = model.mouse |> mouseToWebGL model.size
                    , color = vec3 ((model.time / 100) |> fmod 360 |> degrees) 0.8 1.0
                    , pointSize = 400
                    }
                        :: (model.circles
                                |> List.take maxDepth
                                |> List.map
                                    (\c ->
                                        let
                                            point3 =
                                                vec3 (Vec2.getX c.point) (Vec2.getY c.point) 1

                                            newPoint =
                                                Mat4.transform transform point3
                                        in
                                        { c | point = vec2 (Vec3.getX newPoint) (Vec3.getY newPoint), pointSize = c.pointSize * factor }
                                    )
                           )
            in
            ( { model
                | time = model.time + time
                , circles = circles
                , angle = angle
                , factor = factor
              }
            , Cmd.none
            )

        WindowResize x y ->
            ( { model | size = { x = toFloat x, y = toFloat y } }, Cmd.none )

        MouseMoved point ->
            ( { model | mouse = point }, Cmd.none )

        KeyDown key ->
            case key of
                "w" ->
                    ( { model | w = True }, Cmd.none )

                "a" ->
                    ( { model | a = True }, Cmd.none )

                "s" ->
                    ( { model | s = True }, Cmd.none )

                "d" ->
                    ( { model | d = True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyUp key ->
            case key of
                "w" ->
                    ( { model | w = False }, Cmd.none )

                "a" ->
                    ( { model | a = False }, Cmd.none )

                "s" ->
                    ( { model | s = False }, Cmd.none )

                "d" ->
                    ( { model | d = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta UpdateTime
        , Browser.Events.onResize WindowResize
        , Browser.Events.onMouseMove mouseMoved
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        ]


mouseMoved : Decoder Msg
mouseMoved =
    Decode.map MouseMoved
        (Decode.map2
            Point
            (Decode.field "x" Decode.float)
            (Decode.field "y" Decode.float)
        )


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { time = 0.0
      , circles = []
      , size = { x = 0, y = 0 }
      , mouse = { x = 0, y = 0 }
      , factor = 1.1
      , angle = 0.0
      , w = False
      , a = False
      , s = False
      , d = False
      }
    , Task.perform (\v -> WindowResize (floor v.viewport.width) (floor v.viewport.height)) Browser.Dom.getViewport
    )


main : Program Json.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
