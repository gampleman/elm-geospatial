module Example02 exposing (Model, Msg, init, update, view)

import BBox exposing (BBox)
import Browser exposing (Document)
import Coordinates exposing (WGS84)
import FeatureCollection exposing (FeatureCollection)
import Html exposing (Html)
import Http
import Map.Source as Source
import Map.Style
import Mapbox.Element
import Mapbox.Expression as E exposing (float, str)
import Mapbox.Layer as Layer exposing (Layer)
import Mapbox.Source as Source exposing (Source)
import Mapbox.Style exposing (Style)
import RainfallStation
import Station exposing (Station)
import WaterLevelStation


type alias Model =
    { rainfall : FeatureCollection WGS84 RainfallStation.Status
    , waterLevel : FeatureCollection WGS84 WaterLevelStation.Status
    }


type Msg
    = Loaded (Result Http.Error (FeatureCollection WGS84 Station))


getData : Cmd Msg
getData =
    Http.get
        { url = "https://data.easos.my/geoserver/easos-flooding/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=easos-flooding:rainfall_latest&outputFormat=application%2Fjson"
        , expect = Http.expectJson Loaded (FeatureCollection.decoder Station.decoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loaded (Ok data) ->
            ( process data, Cmd.none )

        _ ->
            ( model, Cmd.none )


malaysiaExtent : BBox
malaysiaExtent =
    BBox.fromExtrema { minLng = 98.085756871, minLat = -0.633131415201, maxLng = 121.181903925, maxLat = 8.4805288332 }


process : FeatureCollection WGS84 Station -> Model
process data =
    { waterLevel =
        data
            |> FeatureCollection.filterMap Station.getWaterLevel
            |> FeatureCollection.filterMap WaterLevelStation.getStatus
    , rainfall =
        data
            |> FeatureCollection.filterMap Station.getRainfall
            |> FeatureCollection.filterMap RainfallStation.getStatus
            |> FeatureCollection.voronoi malaysiaExtent
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { rainfall = [], waterLevel = [] }, getData )


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
    Map.Style.light (sources model) layers


sources : Model -> List Source
sources model =
    [ RainfallStation.source "rainfall" model.rainfall
    , WaterLevelStation.source "waterlevel" model.waterLevel
    ]


layers : List Layer
layers =
    [ Layer.fill "rainfall-voronoi"
        "rainfall"
        [ E.getProperty (str "status")
            |> E.matchesStr
                [ ( "no-rain", E.rgba 73 143 226 0.5 )
                , ( "light", E.rgba 125 210 33 0.9 )
                , ( "moderate", E.rgba 255 239 0 0.9 )
                , ( "heavy", E.rgba 255 155 0 0.9 )
                , ( "very-heavy", E.rgba 255 0 18 0.9 )
                ]
                (E.rgba 49 93 107 0.2)
            |> Layer.fillColor
        , Layer.fillOutlineColor (E.rgba 250 250 250 1)
        ]
    , Layer.circle "waterlevel-points"
        "waterlevel"
        [ E.getProperty (str "status")
            |> E.matchesStr
                [ ( "normal", E.rgba 125 210 33 1 )
                , ( "alert", E.rgba 255 239 0 1 )
                , ( "danger", E.rgba 255 0 18 1 )
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
