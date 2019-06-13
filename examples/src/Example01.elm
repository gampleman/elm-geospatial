module Example01 exposing (Model, init, update, view)

import Browser exposing (Document)
import Html exposing (Html)
import Map.Style
import Mapbox.Element
import Mapbox.Expression as E exposing (float, str)
import Mapbox.Layer as Layer exposing (Layer)
import Mapbox.Source as Source exposing (Source)
import Mapbox.Style exposing (Style)


type alias Model =
    {}


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Flood Example"
    , body = [ mapView model ]
    }


mapView : Model -> Html Msg
mapView model =
    Mapbox.Element.map [] (buildStyle model)


buildStyle : Model -> Style
buildStyle model =
    Map.Style.light
        [ Source.geoJSONFromUrl "flooding" "https://data.easos.my/geoserver/easos-flooding/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=easos-flooding:rainfall_latest&outputFormat=application%2Fjson" [] ]
        [ Layer.circle "points"
            "flooding"
            [ E.getProperty (str "stationtype")
                |> E.matchesStr
                    [ ( "WL", float 1 )
                    , ( "RF", float 3 )
                    , ( "RF & WL", float 5 )
                    ]
                    (float 1)
                |> Layer.circleRadius
            , [ E.getProperty (str "waterlevelmsg"), E.getProperty (str "rainfallmsg") ]
                |> E.coalesce
                |> E.matchesStr
                    [ ( "LIGHT", E.rgba 125 210 33 1 )
                    , ( "NORMAL", E.rgba 125 210 33 1 )
                    , ( "MODERATE", E.rgba 255 239 0 1 )
                    , ( "ALERT", E.rgba 255 239 0 1 )
                    , ( "HEAVY", E.rgba 255 155 0 1 )
                    , ( "WARNING", E.rgba 255 155 0 1 )
                    , ( "VERY HEAVY", E.rgba 255 0 18 1 )
                    , ( "DANGER", E.rgba 255 0 18 1 )
                    , ( "NODATA", E.rgba 49 93 107 0.2 )
                    , ( "OFF", E.rgba 49 93 107 0.2 )
                    ]
                    (E.rgba 49 93 107 0.2)
                |> Layer.circleColor
            ]
        ]


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
