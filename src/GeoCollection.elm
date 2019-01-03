module GeoCollection exposing (GeoCollection, advancedDecoder, decoder, encode)

{-| This represents a collection of geographical data points.
-}

import Angle
import Coordinates exposing (WGS84)
import Feature exposing (Feature(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import LineString exposing (LineString(..))
import Point exposing (Point(..))
import Polygon exposing (LinearRing(..), Polygon(..))


type alias GeoCollection coordinates a =
    List (Feature coordinates a)



-- Maps


mapCoordinates : (a -> b) -> GeoCollection a props -> GeoCollection b props
mapCoordinates f =
    List.map
        (\feature ->
            case feature of
                Points points props ->
                    Points (List.map (\(Point coord) -> Point (f coord)) points) props

                LineStrings ls props ->
                    LineStrings (List.map (\(LineString a b cs) -> LineString (f a) (f b) (List.map f cs)) ls) props

                Polygons polys props ->
                    let
                        mapLR (LinearRing a b c es) =
                            LinearRing (f a) (f b) (f c) (List.map f es)
                    in
                    Polygons (List.map (\(Polygon outer inners) -> Polygon (mapLR outer) (List.map mapLR inners)) polys) props
        )



-- Decoding


decoder : Decoder a -> Decoder (GeoCollection WGS84 a)
decoder propertiesDecoder =
    strictDecoder Coordinates.wgs84Decoder (Decode.field "properties" propertiesDecoder)


type alias DecoderOptions coord a =
    { featureDecoder : Decoder a
    , coordinateDecoder : Decoder coord
    }


advancedDecoder : DecoderOptions coord a -> Decoder (GeoCollection coord a)
advancedDecoder { featureDecoder, coordinateDecoder } =
    strictDecoder coordinateDecoder featureDecoder


strictDecoder : Decoder coordinates -> Decoder a -> Decoder (GeoCollection coordinates a)
strictDecoder coordDecoder propDecoder =
    Decode.field "features" <| Decode.list (strictFeatureDecoder coordDecoder propDecoder)


strictFeatureDecoder : Decoder coordinates -> Decoder a -> Decoder (Feature coordinates a)
strictFeatureDecoder coordDecoder propDecoder =
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
                                            Decode.map (\points -> Points points prop) (strictPointDecoder coordDecoder)

                                        "MultiPoint" ->
                                            Decode.map (\points -> Points points prop) (strictMultiPointDecoder coordDecoder)

                                        "LineString" ->
                                            Decode.map (\lines -> LineStrings lines prop) (strictLineStringDecoder coordDecoder)

                                        "MultiLineString" ->
                                            Decode.map (\lines -> LineStrings lines prop) (strictMultiLineStringDecoder coordDecoder)

                                        "Polygon" ->
                                            Decode.map (\polygons -> Polygons polygons prop) (strictPolygonDecoder coordDecoder)

                                        "MultiPolygon" ->
                                            Decode.map (\polygons -> Polygons polygons prop) (strictMultiPolygonDecoder coordDecoder)

                                        _ ->
                                            Decode.fail ("The type '" ++ t ++ "' is not supported.")
                                )

                    _ ->
                        Decode.fail ("Unexpected type '" ++ t0 ++ "'")
            )


strictPointDecoder : Decoder coordinates -> Decoder (List (Point coordinates))
strictPointDecoder coordDecoder =
    Decode.map List.singleton <| Decode.map Point <| Decode.at [ "geometry", "coordinates" ] coordDecoder


strictMultiPointDecoder : Decoder coordinates -> Decoder (List (Point coordinates))
strictMultiPointDecoder coordDecoder =
    Decode.map (List.map Point) <| Decode.at [ "geometry", "coordinates" ] <| Decode.list coordDecoder


strictLineStringHelper : Decoder coordinates -> Decoder (LineString coordinates)
strictLineStringHelper coordDecoder =
    Decode.list coordDecoder
        |> Decode.andThen
            (\l ->
                case l of
                    a :: b :: rest ->
                        Decode.succeed (LineString a b rest)

                    _ ->
                        Decode.fail "Insufficient coordinates for a line string"
            )


strictLineStringDecoder : Decoder coordinates -> Decoder (List (LineString coordinates))
strictLineStringDecoder coordDecoder =
    Decode.map List.singleton <| Decode.at [ "geometry", "coordinates" ] (strictLineStringHelper coordDecoder)


strictMultiLineStringDecoder : Decoder coordinates -> Decoder (List (LineString coordinates))
strictMultiLineStringDecoder coordDecoder =
    Decode.at [ "geometry", "coordinates" ] <| Decode.list (strictLineStringHelper coordDecoder)


strictPolygonDecoderHelper : List (LinearRing coordinates) -> Decoder (Polygon coordinates)
strictPolygonDecoderHelper positions =
    case positions of
        [] ->
            Decode.fail "Not enough coordinates for polygon"

        outer :: holes ->
            Decode.succeed (Polygon outer holes)


strictDecodeLinearRing : Decoder coordinates -> Decoder (LinearRing coordinates)
strictDecodeLinearRing coordDecoder =
    Decode.list coordDecoder
        |> Decode.andThen
            (\l ->
                case l of
                    a :: b :: c :: rest ->
                        case List.reverse rest of
                            [] ->
                                Decode.fail "Not enough coordinates for a linear ring"

                            h :: t ->
                                if h == a then
                                    Decode.succeed (LinearRing a b c (List.reverse t))

                                else
                                    Decode.fail "First and last coordinate must match in a polygon's linear ring"

                    _ ->
                        Decode.fail "Not enough coordinates for a linear ring"
            )


strictPolygonDecoder : Decoder coordinates -> Decoder (List (Polygon coordinates))
strictPolygonDecoder coordDecoder =
    Decode.map List.singleton <| Decode.andThen strictPolygonDecoderHelper <| Decode.at [ "geometry", "coordinates" ] <| Decode.list (strictDecodeLinearRing coordDecoder)


strictMultiPolygonDecoder : Decoder coordinates -> Decoder (List (Polygon coordinates))
strictMultiPolygonDecoder coordDecoder =
    Decode.list (strictDecodeLinearRing coordDecoder)
        |> Decode.andThen strictPolygonDecoderHelper
        |> Decode.list
        |> Decode.at [ "geometry", "coordinates" ]



-- Encoding


encode : (a -> Value) -> (a -> List ( String, Value )) -> GeoCollection WGS84 a -> Value
encode propsEncode foreignEncode geoCollection =
    Encode.object
        [ ( "type", Encode.string "FeatureCollection" )
        , ( "features", Encode.list (encodeItem propsEncode foreignEncode) geoCollection )
        ]


encodeItem : (a -> Value) -> (a -> List ( String, Value )) -> Feature WGS84 a -> Value
encodeItem propsEncode foreignEncode feature =
    foreignEncode (Feature.properties feature)
        ++ [ ( "type", Encode.string "Feature" )
           , ( "properties", propsEncode (Feature.properties feature) )
           , ( "geometry", encodeGeometries feature )
           ]
        |> Encode.object


encodePosition : WGS84 -> Value
encodePosition { lng, lat } =
    Encode.list Encode.float [ Angle.inDegrees lng, Angle.inDegrees lat ]


encodeGeometries : Feature WGS84 a -> Value
encodeGeometries feature =
    case feature of
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


encodeLinearRing : LinearRing WGS84 -> Value
encodeLinearRing (LinearRing a b c rest) =
    Encode.list encodePosition (a :: b :: c :: rest ++ [ a ])
