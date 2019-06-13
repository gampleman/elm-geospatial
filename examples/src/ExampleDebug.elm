module ExampleDebug exposing (Model, Msg, init, update, view)

import BBox exposing (BBox)
import Browser exposing (Document)
import Coordinates exposing (WGS84)
import Feature exposing (Feature(..))
import FeatureCollection exposing (FeatureCollection)
import Html exposing (Html)
import Http
import Http.Tasks
import Json.Decode as Decode
import Json.Encode
import Map.Source as Source
import Map.Style
import Mapbox.Element
import Mapbox.Expression as E exposing (float, ln, str)
import Mapbox.Layer as Layer exposing (Layer)
import Mapbox.Source as Source exposing (Source)
import Mapbox.Style exposing (Style)
import Platform exposing (Task)
import Polygon
import RainfallStation
import Station exposing (Station)
import Task
import WaterLevelStation


type alias Model =
    { test : FeatureCollection WGS84 ()
    , malaysia : FeatureCollection WGS84 ()
    }


type Msg
    = Noop


malaysia =
    """{
  "type": "Polygon",
  "coordinates": [
    [
      [
        101.6455078125,
        2.6160253746090376
      ],
      [
        101.54939889907837,
        2.654994159422259
      ],
      [
        101.51746988296509,
        2.6716701956277262
      ],
      [
        101.50483131408691,
        2.680908365074842
      ],
      [
        101.48663520812988,
        2.6972196724722437
      ],
      [
        101.47663593292236,
        2.7077865394267917
      ],
      [
        101.46775245666504,
        2.719725059131676
      ],
      [
        101.45719528198242,
        2.7363787751057203
      ],
      [
        101.4547061920166,
        2.7405582428529556
      ],
      [
        101.45082235336304,
        2.750267411559591
      ],
      [
        101.44356966018677,
        2.7636200685132195
      ],
      [
        101.43303394317627,
        2.780273171662529
      ],
      [
        101.41359329223633,
        2.8022197591734823
      ],
      [
        101.40136241912842,
        2.810835397054305
      ],
      [
        101.3924789428711,
        2.8158290088102973
      ],
      [
        101.3818359375,
        2.8199653319541937
      ],
      [
        101.3818359375,
        2.8552627843665874
      ],
      [
        101.6455078125,
        2.8552627843665874
      ],
      [
        101.6455078125,
        2.6160253746090376
      ]
    ]
  ]
}"""


test =
    """ {
"type": "Polygon",
"coordinates": [
[
  [
    100.8984375,
    2.6493354018336532
  ],
  [
    101.39883041381836,
    2.8456616412253197
  ],
  [
    101.4012336730957,
    2.845833090912066
  ],
  [
    101.51538848876953,
    2.7462165759303048
  ],
  [
    100.8984375,
    1.8573940122606416
  ],
  [
    100.8984375,
    2.6493354018336532
  ]
]
]
}"""


decodePoly : String -> FeatureCollection WGS84 ()
decodePoly input =
    """{"type": "FeatureCollection", "features": [{"type": "Feature", "properties": {}, "geometry": """
        ++ input
        ++ "}]}"
        |> Decode.decodeString (FeatureCollection.decoder (Decode.succeed ()))
        |> Result.withDefault []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


clipWith : FeatureCollection WGS84 () -> FeatureCollection WGS84 () -> FeatureCollection WGS84 ()
clipWith outline data =
    let
        mask =
            List.concatMap
                (\feat ->
                    case feat of
                        Polygons polys _ ->
                            List.map (\poly -> ( Polygon.bbox poly, poly )) polys

                        _ ->
                            []
                )
                outline
    in
    List.map
        (\feature ->
            case feature of
                Polygons polys metadata ->
                    Polygons (List.concatMap (performClip mask) polys) metadata

                _ ->
                    feature
        )
        data


performClip mask apoly =
    let
        aBbox =
            Polygon.bbox apoly
    in
    List.filterMap
        (\( bBbox, bpoly ) ->
            if BBox.overlap aBbox bBbox then
                Polygon.intersection apoly bpoly

            else
                Nothing
        )
        mask


init : () -> ( Model, Cmd Msg )
init flags =
    let
        malay =
            decodePoly malaysia

        clipped =
            clipWith malay (decodePoly test)
    in
    ( { test = clipped, malaysia = malay }, Cmd.none )


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
    [ Source.geoJSONFromValue "malaysia" [ Source.tolerance 0 ] (FeatureCollection.encode (always Json.Encode.null) model.malaysia)
    , Source.geoJSONFromValue "test" [ Source.tolerance 0 ] (FeatureCollection.encode (always Json.Encode.null) model.test)
    ]


layers : List Layer
layers =
    [ Layer.fill "malaysia"
        "malaysia"
        [ Layer.fillColor (E.rgba 125 226 165 0.38)
        , Layer.fillOutlineColor (E.rgba 250 150 150 1)
        ]
    , Layer.fill "test"
        "test"
        [ Layer.fillColor (E.rgba 41 35 161 0.5)
        , Layer.fillOutlineColor (E.rgba 255 255 255 1)
        ]
    , Layer.circle "malaysia-points"
        "malaysia"
        [ Layer.circleColor (E.rgba 0 0 0 1)
        , Layer.circleRadius (E.int 1)
        , Layer.circleStrokeColor (E.rgba 250 150 150 1)
        , Layer.circleStrokeWidth (E.int 1)
        ]
    , Layer.circle "test-points"
        "test"
        [ Layer.circleColor (E.rgba 0 0 0 1)
        , Layer.circleRadius (E.int 1)
        , Layer.circleStrokeColor (E.rgba 255 255 255 1)
        , Layer.circleStrokeWidth (E.int 1)
        ]
    ]


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
