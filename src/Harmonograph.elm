module Main exposing (main)

import AnimationFrame
import Color exposing (Color)
import Html exposing (Html)
import Svg exposing (Attribute, Svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import Time exposing (Time)


type alias Point =
    { x : Float
    , y : Float
    }


type alias Model =
    { points : List Point
    , time : Time
    }


type Msg
    = UpdateTime Time


size : Point
size =
    { x = 500
    , y = 500
    }


center : Point
center =
    { x = size.x / 2, y = size.y / 2 }


pointsToAttributes : ( Point, Point ) -> List (Attribute msg)
pointsToAttributes ( p1, p2 ) =
    [ p1.x |> toString |> x1
    , p1.y |> toString |> y1
    , p2.x |> toString |> x2
    , p2.y |> toString |> y2
    , stroke "white"
    , strokeWidth "1"
    ]


renderPoints : List Point -> Svg msg
renderPoints points =
    Svg.g []
        (points
            |> List.drop 1
            |> List.map2 (,) points
            |> List.map pointsToAttributes
            |> List.map (\attrs -> Svg.line attrs [])
        )


view : Model -> Html msg
view model =
    Svg.svg
        [ size.x |> toString |> width
        , size.y |> toString |> height
        , "0 0 " ++ toString size.x ++ " " ++ toString size.y |> viewBox
        ]
        [ Svg.rect [ fill "000000", x "0", y "0", height "500", width "500" ] []
        , renderPoints model.points
        ]


type alias Constants =
    { a : Float, f : Float, p : Float, d : Float }


harmonograph t const =
    let
        e =
            2.718281
    in
        const.a * sin (t * const.f + const.p) * e ^ (-const.d * t)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTime time ->
            -- f1=2.01 f2=3 f3=3 f4=2
            -- d1=0.0085 d2=0 d3=0.065 d4=0 p1=0 p2=7 pi/16 p3=0 p4=0
            let
                t =
                    model.time + 0.1

                one =
                    { a = 100.0
                    , f = 2.01
                    , p = 0
                    , d = 0.0085
                    }

                two =
                    { a = 100.0
                    , f = 3.0
                    , p = 7 * pi / 16
                    , d = 0.0
                    }

                three =
                    { a = 100.0
                    , f = 3.0
                    , p = 0
                    , d = 0.065
                    }

                four =
                    { a = 100.0
                    , f = 2.0
                    , p = 0
                    , d = 0.0
                    }

                x =
                    harmonograph t one + harmonograph t two

                y =
                    harmonograph t three + harmonograph t four

                points =
                    -- model.points
                    Point (x + center.x) (y + center.y) :: model.points
            in
                ( { model | time = t, points = points }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs UpdateTime


main =
    Html.program
        { init = ( { time = 0, points = [] }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
