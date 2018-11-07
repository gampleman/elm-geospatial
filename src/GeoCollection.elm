module GeoCollection exposing (GeoCollection, encode, strictDecoder)

{-| This represents a collection of geographical data points.
-}

import GeoItem exposing (GeoItem(..))
import GeoPosition exposing (GeoPosition)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import LineString exposing (LineString(..))
import Point exposing (Point(..))
import Polygon exposing (LinearRing(..), Polygon(..))


type alias GeoCollection a =
    List (GeoItem a)



-- Decoding


strictDecoder : Decoder a -> Decoder (GeoCollection a)
strictDecoder propDecoder =
    Decode.field "features" <| Decode.list (strictFeatureDecoder propDecoder)


strictFeatureDecoder : Decoder a -> Decoder (GeoItem a)
strictFeatureDecoder propDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t0 ->
                case t0 of
                    "Feature" ->
                        Decode.map2 Tuple.pair (Decode.at [ "geometry", "type" ] Decode.string) propDecoder
                            |> Decode.andThen
                                (\( t, prop ) ->
                                    case t of
                                        "Point" ->
                                            Decode.map (\points -> Points points prop) strictPointDecoder

                                        "MultiPoint" ->
                                            Decode.map (\points -> Points points prop) strictMultiPointDecoder

                                        "LineString" ->
                                            Decode.map (\lines -> LineStrings lines prop) strictLineStringDecoder

                                        "MultiLineString" ->
                                            Decode.map (\lines -> LineStrings lines prop) strictMultiLineStringDecoder

                                        "Polygon" ->
                                            Decode.map (\polygons -> Polygons polygons prop) strictPolygonDecoder

                                        "MultiPolygon" ->
                                            Decode.map (\polygons -> Polygons polygons prop) strictMultiPolygonDecoder

                                        _ ->
                                            Decode.fail ("The type '" ++ t ++ "' is not supported.")
                                )

                    _ ->
                        Decode.fail ("Unexpected type '" ++ t0 ++ "'")
            )


strictDecodePosition : Decoder GeoPosition
strictDecodePosition =
    Decode.list Decode.float
        |> Decode.andThen
            (\l ->
                case l of
                    [ lng, lat ] ->
                        Decode.succeed { lng = lng, lat = lat }

                    _ ->
                        Decode.fail "Wrong number of coordinates"
            )


strictPointDecoder : Decoder (List Point)
strictPointDecoder =
    Decode.map List.singleton <| Decode.map Point <| Decode.at [ "geometry", "coordinates" ] strictDecodePosition


strictMultiPointDecoder : Decoder (List Point)
strictMultiPointDecoder =
    Decode.map (List.map Point) <| Decode.at [ "geometry", "coordinates" ] <| Decode.list strictDecodePosition


strictLineStringHelper : Decoder LineString
strictLineStringHelper =
    Decode.list strictDecodePosition
        |> Decode.andThen
            (\l ->
                case l of
                    a :: b :: rest ->
                        Decode.succeed (LineString a b rest)

                    _ ->
                        Decode.fail "Insufficient coordinates for a line string"
            )


strictLineStringDecoder : Decoder (List LineString)
strictLineStringDecoder =
    Decode.map List.singleton <| Decode.at [ "geometry", "coordinates" ] strictLineStringHelper


strictMultiLineStringDecoder : Decoder (List LineString)
strictMultiLineStringDecoder =
    Decode.at [ "geometry", "coordinates" ] <| Decode.list strictLineStringHelper


strictPolygonDecoderHelper : List LinearRing -> Decoder Polygon
strictPolygonDecoderHelper positions =
    case positions of
        [] ->
            Decode.fail "Not enough coordinates for polygon"

        outer :: holes ->
            Decode.succeed (Polygon outer holes)


strictDecodeLinearRing : Decoder LinearRing
strictDecodeLinearRing =
    Decode.list strictDecodePosition
        |> Decode.andThen
            (\l ->
                case l of
                    a :: b :: c :: d :: rest ->
                        Decode.succeed (LinearRing a b c d rest)

                    _ ->
                        Decode.fail "Not enough coordinates for a linear ring"
            )


strictPolygonDecoder : Decoder (List Polygon)
strictPolygonDecoder =
    Decode.map List.singleton <| Decode.andThen strictPolygonDecoderHelper <| Decode.at [ "geometry", "coordinates" ] <| Decode.list strictDecodeLinearRing


strictMultiPolygonDecoder : Decoder (List Polygon)
strictMultiPolygonDecoder =
    Decode.list strictDecodeLinearRing
        |> Decode.andThen strictPolygonDecoderHelper
        |> Decode.list
        |> Decode.at [ "geometry", "coordinates" ]



-- Encoding


encode : (a -> Value) -> (a -> List ( String, Value )) -> GeoCollection a -> Value
encode propsEncode foreignEncode geoCollection =
    Encode.object
        [ ( "type", Encode.string "FeatureCollection" )
        , ( "features", Encode.list (encodeItem propsEncode foreignEncode) geoCollection )
        ]


encodeItem : (a -> Value) -> (a -> List ( String, Value )) -> GeoItem a -> Value
encodeItem propsEncode foreignEncode geoitem =
    foreignEncode (GeoItem.properties geoitem)
        ++ [ ( "type", Encode.string "Feature" )
           , ( "properties", propsEncode (GeoItem.properties geoitem) )
           , ( "geometry", encodeGeometries geoitem )
           ]
        |> Encode.object


encodePosition : GeoPosition -> Value
encodePosition { lng, lat } =
    Encode.list Encode.float [ lng, lat ]


encodeGeometries : GeoItem a -> Value
encodeGeometries geoitem =
    case geoitem of
        Points points _ ->
            Encode.object
                [ ( "type", Encode.string "MultiPoint" )
                , ( "coordinates", Encode.list (\(Point position) -> encodePosition position) points )
                ]

        LineStrings lineStrings _ ->
            Encode.object
                [ ( "type", Encode.string "MultiLineString" )
                , ( "coordinates", Encode.list (\(LineString a b rest) -> Encode.list encodePosition (a :: b :: rest)) lineStrings )
                ]

        Polygons polygons _ ->
            Encode.object
                [ ( "type", Encode.string "MultiPolygon" )
                , ( "coordinates", Encode.list (\(Polygon outer holes) -> Encode.list identity (encodeLinearRing outer :: List.map encodeLinearRing holes)) polygons )
                ]


encodeLinearRing : LinearRing -> Value
encodeLinearRing (LinearRing a b c d rest) =
    Encode.list encodePosition (a :: b :: c :: d :: rest)
