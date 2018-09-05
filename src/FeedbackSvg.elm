module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Json
import Task
import TypedSvg as Svg exposing (circle, g, rect)
import TypedSvg.Attributes exposing (fill, stroke, style, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Color as Color
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


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


type alias Model =
    { time : Float
    , elements : List (List (Svg Msg) -> Svg Msg)
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
    Svg.svg
        [ width model.size.x
        , height model.size.y
        , viewBox 0 0 model.size.x model.size.y
        ]
        [ rect
            [ fill (Fill Color.black)
            , x 0
            , y 0
            , height model.size.y
            , width model.size.x
            ]
            []
        , model.elements
            |> List.foldr (\f s -> f [ s ]) (g [] [])
        ]


fmod : Float -> Float -> Float
fmod modulus x =
    x - modulus * toFloat (floor (x / modulus))


maxDepth : Int
maxDepth =
    250


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTime time ->
            let
                angle =
                    if model.a then
                        model.angle + 1

                    else if model.d then
                        model.angle - 1

                    else
                        model.angle

                factor =
                    if model.w then
                        model.factor + 0.01

                    else if model.s then
                        model.factor - 0.01

                    else
                        model.factor

                color =
                    Color.hsl ((model.time / 10) |> fmod 360 |> degrees) 0.5 0.5

                center =
                    half model.size

                globalTransform =
                    [ Translate
                        (-center.x * (model.factor - 1))
                        (-center.y * (model.factor - 1))
                    , Scale model.factor model.factor
                    , Rotate model.angle center.x center.y
                    ]

                elements =
                    (\l ->
                        g []
                            [ circle
                                [ fill (Fill color)
                                , cx model.mouse.x
                                , cy model.mouse.y
                                , r 50
                                ]
                                []
                            , g [ transform globalTransform ] l
                            ]
                    )
                        :: List.take maxDepth model.elements
            in
            ( { model
                | elements = elements
                , time = model.time + time
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
      , elements = [ g [] ]
      , size = { x = 0, y = 0 }
      , mouse = { x = 0, y = 0 }
      , factor = 1.0
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
