module Main exposing (main)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Color.Interpolate exposing (Space(RGB), interpolate)
import Html exposing (Html)
import Svg exposing (Svg, circle, g, rect, svg, text, text_)
import Svg.Attributes exposing (cx, cy, fill, height, r, viewBox, width, x, y)


type alias Point =
    { x : Float
    , y : Float
    }


colorPoints : List Color
colorPoints =
    [ Color.rgb 102 37 6
    , Color.rgb 153 52 4
    , Color.rgb 204 76 2
    , Color.rgb 236 112 20
    , Color.rgb 254 153 41
    , Color.rgb 254 196 79
    , Color.rgb 254 228 145
    , Color.rgb 255 247 188
    , Color.rgb 255 255 229
    ]


interpol8 : Color -> Color -> List Color
interpol8 c1 c2 =
    List.range 0 8
        |> List.map toFloat
        |> List.map (\x -> x / 8)
        |> List.map (interpolate RGB c2 c1)


colors : List Color
colors =
    colorPoints
        |> List.map2 (,) (List.drop 1 colorPoints)
        |> List.concatMap (uncurry interpol8)


size : Point
size =
    { x = 500
    , y = 500
    }


center : Point
center =
    { x = size.x / 2, y = size.y / 2 }


makePoints : Int -> List Point
makePoints n =
    List.range 1 n
        |> List.map toFloat
        |> List.map (\m -> ( sqrt m, 2.4 * m ))
        |> List.map fromPolar
        |> List.map (uncurry Point)


floret : Int -> Point -> Svg msg
floret n p =
    let
        h =
            floor (1.4 * sqrt (toFloat n)) % List.length colors

        color =
            List.drop h colors
                |> List.head
                |> Maybe.withDefault Color.white
    in
        circle
            [ color |> colorToHex |> fill
            , r "3.1"
            , p.x |> toString |> cx
            , p.y |> toString |> cy
            ]
            []


sunflower : Int -> Point -> Svg msg
sunflower n c =
    makePoints n
        |> List.map (\p -> { x = 5 * p.x + c.x, y = 5 * p.y + c.y })
        |> List.indexedMap floret
        |> g []


main : Html msg
main =
    svg
        [ size.x |> toString |> width
        , size.y |> toString |> height
        , "0 0 " ++ toString size.x ++ " " ++ toString size.y |> viewBox
        ]
        [ rect [ fill "000000", x "0", y "0", height "500", width "500" ] []
        , sunflower 2000 center
        ]
