module GeoCollectionTest exposing (encodingTest, strictDecodingTest)

import Angle
import Coordinates
import Expect exposing (Expectation)
import Feature exposing (Feature(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Fuzzers exposing (geoCollectionFuzzer)
import GeoCollection
import Json.Decode
import Json.Encode
import LineString exposing (LineString(..))
import Point exposing (Point(..))
import Polygon exposing (LinearRing(..), Polygon(..))
import Test exposing (Test, describe, fuzz, test)
import TestHelpers exposing (equalWithinTolerance)


decode =
    Json.Decode.decodeString (GeoCollection.decoder (Json.Decode.map (always ()) Json.Decode.value))


strictDecodingTest : Test
strictDecodingTest =
    describe "Strict Decoding"
        [ test "fails for empty GeoJSON" <|
            \() ->
                decode """{}"""
                    |> Expect.err
        , test "succeeds for empty FeatureCollection" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": []
                }"""
                    |> Expect.equal (Ok [])
        , test "succeeds for a point" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "properties": null,
                        "geometry": {
                            "type": "Point",
                            "coordinates": [32, 21]
                        }
                    }]
                }"""
                    |> Expect.equal (Ok [ Points [ Point.new { lng = 32, lat = 21 } ] () ])
        , test "succeeds for a multipoint" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "properties": null,
                        "geometry": {
                            "type": "MultiPoint",
                            "coordinates": [[32, 21], [21, 31]]
                        }
                    }]
                }"""
                    |> Expect.equal (Ok [ Points [ Point.new { lng = 32, lat = 21 }, Point.new { lng = 21, lat = 31 } ] () ])
        , test "fails for a point with too few coordinates" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "properties": null,
                        "geometry": {
                            "type": "Point",
                            "coordinates": [32]
                        }
                    }]
                }"""
                    |> Expect.err
        , test "fails for a point with too many coordinates" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "properties": null,
                        "geometry": {
                            "type": "Point",
                            "coordinates": [32, 43, 23]
                        }
                    }]
                }"""
                    |> Expect.err
        , test "succeeds for a linestring" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "properties": null,
                        "geometry": {
                            "type": "LineString",
                            "coordinates": [[32, 21], [21, 31]]
                        }
                    }]
                }"""
                    |> Expect.equal
                        (Ok
                            [ LineStrings
                                [ LineString { lng = Angle.degrees 32, lat = Angle.degrees 21 }
                                    { lng = Angle.degrees 21, lat = Angle.degrees 31 }
                                    []
                                ]
                                ()
                            ]
                        )
        , test "succeeds for a multilinestring" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "properties": null,
                        "geometry": {
                            "type": "MultiLineString",
                            "coordinates": [[[32, 21], [21, 31]]]
                        }
                    }]
                }"""
                    |> Expect.equal
                        (Ok
                            [ LineStrings
                                [ LineString { lng = Angle.degrees 32, lat = Angle.degrees 21 }
                                    { lng = Angle.degrees 21, lat = Angle.degrees 31 }
                                    []
                                ]
                                ()
                            ]
                        )
        , test "fails for a linestring with insufficient coordinates" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "properties": null,
                        "geometry": {
                            "type": "LineString",
                            "coordinates": [[32, 21]]
                        }
                    }]
                }"""
                    |> Expect.err
        , test "succeeds for a simple polygon" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "properties": null,
                        "geometry": {
                            "type": "Polygon",
                            "coordinates": [[[32, 21], [21, 31], [23, 32], [32, 21]]]
                        }
                    }]
                }"""
                    |> Expect.equal
                        (Ok
                            [ Polygons
                                [ Polygon
                                    (LinearRing { lng = Angle.degrees 32, lat = Angle.degrees 21 }
                                        { lng = Angle.degrees 21, lat = Angle.degrees 31 }
                                        { lng = Angle.degrees 23, lat = Angle.degrees 32 }
                                        []
                                    )
                                    []
                                ]
                                ()
                            ]
                        )
        , test "succeeds for a polygon with a hole" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "properties": null,
                        "geometry": {
                            "type": "Polygon",
                            "coordinates": [
                                [[32, 21], [21, 31], [23, 32], [32, 21]],
                                [[1, 1], [2, 2], [3, 3], [4, 4], [1, 1]]
                            ]
                        }
                    }]
                }"""
                    |> Expect.equal
                        (Ok
                            [ Polygons
                                [ Polygon
                                    (LinearRing { lng = Angle.degrees 32, lat = Angle.degrees 21 }
                                        { lng = Angle.degrees 21, lat = Angle.degrees 31 }
                                        { lng = Angle.degrees 23, lat = Angle.degrees 32 }
                                        []
                                    )
                                    [ LinearRing { lng = Angle.degrees 1, lat = Angle.degrees 1 }
                                        { lng = Angle.degrees 2, lat = Angle.degrees 2 }
                                        { lng = Angle.degrees 3, lat = Angle.degrees 3 }
                                        [ { lng = Angle.degrees 4, lat = Angle.degrees 4 } ]
                                    ]
                                ]
                                ()
                            ]
                        )
        , test "fails for a polygon with insufficient coordinates" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "properties": null,
                        "geometry": {
                            "type": "Polygon",
                            "coordinates": [[[32, 21], [21, 31], [23, 32]]]
                        }
                    }]
                }"""
                    |> Expect.err
        , test "succeeds for a multipolygon" <|
            \() ->
                decode """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "properties": null,
                        "geometry": {
                            "type": "MultiPolygon",
                            "coordinates": [[[[32, 21], [21, 31], [23, 32], [32, 21]]]]
                        }
                    }]
                }"""
                    |> Expect.equal
                        (Ok
                            [ Polygons
                                [ Polygon
                                    (LinearRing { lng = Angle.degrees 32, lat = Angle.degrees 21 }
                                        { lng = Angle.degrees 21, lat = Angle.degrees 31 }
                                        { lng = Angle.degrees 23, lat = Angle.degrees 32 }
                                        []
                                    )
                                    []
                                ]
                                ()
                            ]
                        )
        , test "can decode properties and attributes" <|
            \() ->
                let
                    propsDecoder =
                        Json.Decode.map2 (\id name -> { id = id, name = name })
                            (Json.Decode.at [ "id" ] Json.Decode.int)
                            (Json.Decode.at [ "properties", "name" ] Json.Decode.string)
                in
                """{
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "id": 32,
                        "properties": {
                            "name": "John Doe"
                        },
                        "geometry": {
                            "type": "Point",
                            "coordinates": [32, 21]
                        }
                    }]
                }"""
                    |> Json.Decode.decodeString
                        (GeoCollection.advancedDecoder
                            { featureDecoder = propsDecoder
                            , coordinateDecoder = Coordinates.wgs84Decoder
                            }
                        )
                    |> Expect.equal (Ok [ Points [ Point.new { lng = 32, lat = 21 } ] { id = 32, name = "John Doe" } ])
        , test "can decode unusual coordinate systems" <|
            \() ->
                let
                    coordinateDecoder =
                        Json.Decode.list Json.Decode.float
                            |> Json.Decode.andThen
                                (\l ->
                                    case l of
                                        [ lng, lat, alt ] ->
                                            Json.Decode.succeed { lng = lng, lat = lat, alt = alt }

                                        _ ->
                                            Json.Decode.fail "wrong number of coordinates"
                                )
                in
                """{
                 "type": "FeatureCollection",
                 "features": [{
                     "type": "Feature",
                     "properties": {
                     },
                     "geometry": {
                         "type": "Point",
                         "coordinates": [32, 21, 3]
                     }
                 }]
             }"""
                    |> Json.Decode.decodeString
                        (GeoCollection.advancedDecoder
                            { featureDecoder = Json.Decode.succeed ()
                            , coordinateDecoder = coordinateDecoder
                            }
                        )
                    |> Expect.equal (Ok [ Points [ Point { lng = 32, lat = 21, alt = 3 } ] () ])
        ]


encode =
    GeoCollection.encode (always Json.Encode.null) (always [])


expectEqualsJson : String -> Json.Encode.Value -> Expectation
expectEqualsJson string val =
    let
        actual =
            Ok val

        expected =
            Json.Decode.decodeString Json.Decode.value string
    in
    if actual == expected then
        actual |> Expect.equal expected

    else
        Json.Encode.encode 4 val |> Expect.equal string


encodingTest : Test
encodingTest =
    describe "encoding"
        [ test "an empty list works" <|
            \() ->
                []
                    |> encode
                    |> expectEqualsJson """
                        {
                            "type": "FeatureCollection",
                            "features": []
                        }
                        """
        , test "points" <|
            \() ->
                [ Points [ Point.new { lng = 32, lat = 21 } ] () ]
                    |> encode
                    |> expectEqualsJson """
                    {
                        "type": "FeatureCollection",
                        "features": [{
                            "type": "Feature",
                            "properties": null,
                            "geometry": {
                                "type": "MultiPoint",
                                "coordinates": [[32, 21]]
                            }
                        }]
                    }
                    """
        , test "line strings" <|
            \() ->
                [ LineStrings [ LineString { lng = Angle.degrees 32, lat = Angle.degrees 21 } { lng = Angle.degrees 21, lat = Angle.degrees 31 } [] ] () ]
                    |> encode
                    |> expectEqualsJson """
                    {
                        "type": "FeatureCollection",
                        "features": [{
                            "type": "Feature",
                            "properties": null,
                            "geometry": {
                                "type": "MultiLineString",
                                "coordinates": [[[32, 21], [21, 31]]]
                            }
                        }]
                    }
                    """
        , test "polygons" <|
            \() ->
                [ Polygons
                    [ Polygon
                        (LinearRing { lng = Angle.degrees 32, lat = Angle.degrees 21 }
                            { lng = Angle.degrees 21, lat = Angle.degrees 31 }
                            { lng = Angle.degrees 23, lat = Angle.degrees 32 }
                            []
                        )
                        [ LinearRing { lng = Angle.degrees 1, lat = Angle.degrees 1 }
                            { lng = Angle.degrees 2, lat = Angle.degrees 2 }
                            { lng = Angle.degrees 3, lat = Angle.degrees 3 }
                            [ { lng = Angle.degrees 4, lat = Angle.degrees 4 } ]
                        ]
                    ]
                    ()
                ]
                    |> encode
                    |> expectEqualsJson """
                    {
                        "type": "FeatureCollection",
                        "features": [{
                            "type": "Feature",
                            "properties": null,
                            "geometry": {
                                "type": "MultiPolygon",
                                "coordinates": [[
                                    [[32, 21], [21, 31], [23, 32], [32, 21]],
                                    [[1, 1], [2, 2], [3, 3], [4, 4], [1, 1]]
                                ]]
                            }
                        }]
                    }
                    """
        , test "can encode properties and attributes" <|
            \() ->
                [ Points [ Point.new { lng = 32, lat = 21 } ] { id = 32, name = "John Doe" } ]
                    |> GeoCollection.encode
                        (\{ name } -> Json.Encode.object [ ( "name", Json.Encode.string name ) ])
                        (\{ id } -> [ ( "id", Json.Encode.int id ) ])
                    |> expectEqualsJson """
                        {
                            "type": "FeatureCollection",
                            "features": [{
                                "type": "Feature",
                                "id": 32,
                                "properties": {
                                    "name": "John Doe"
                                },
                                "geometry": {
                                    "type": "MultiPoint",
                                    "coordinates": [[32, 21]]
                                }
                            }]
                        }
                        """
        , fuzz (geoCollectionFuzzer (Fuzz.constant ())) "rountrips with decoding" <|
            \geocollection ->
                geocollection
                    |> encode
                    |> Json.Decode.decodeValue (GeoCollection.decoder (Json.Decode.map (always ()) Json.Decode.value))
                    |> equalWithinTolerance (Ok geocollection)
        ]
