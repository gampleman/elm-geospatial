module FeatureCollection exposing
    ( FeatureCollection
    , decoder, advancedDecoder, DecoderMode(..)
    , encode, advancedEncode
    , mapCoordinates, mapProperties
    )

{-| A FeatureCollection is just a bunch of stuff in the world our app happens to care
about. This is most likely what you will decode your data into and what you will store in
your model.

@docs FeatureCollection


## GeoJSON

One of the most imporant things you will need to do with these is to get them from
somewhere. One of the most popular formats for geographic data interchange on the
web is [GeoJSON](https://tools.ietf.org/html/rfc7946). This library includes some tools
for working with it:

@docs decoder, advancedDecoder, DecoderMode
@docs encode, advancedEncode


## Mapping (the FP kind)

While nothing is stopping you from using `List.map`, there are some more convenient mapping functions:

@docs mapCoordinates, mapProperties

-}

import Angle
import Coordinates exposing (WGS84)
import Feature exposing (Feature(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import LineString exposing (LineString(..))
import Point exposing (Point(..))
import Polygon exposing (LinearRing(..), Polygon(..))


{-| Importantly, a FeatureCollection is just a type alias over a list of features.
This means that you can easily use normal List functions to deal with these.
-}
type alias FeatureCollection coordinates properties =
    List (Feature coordinates properties)



-- Maps


{-| Transform coordinates. This is useful if you are for example changing coordinate reference system for your data.
-}
mapCoordinates : (a -> b) -> FeatureCollection a props -> FeatureCollection b props
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


{-| Like `List.map`, but allows you to ignore the geometry and operate only on the custom data held with it.
-}
mapProperties : (a -> b) -> FeatureCollection coord a -> FeatureCollection coord b
mapProperties f =
    List.map
        (\feature ->
            case feature of
                Points points props ->
                    Points points (f props)

                LineStrings ls props ->
                    LineStrings ls (f props)

                Polygons polys props ->
                    Polygons polys (f props)
        )



-- Decoding


{-| Decodes a GeoJSON FeatureCollection into WSG84 coordinates. You need to pass in a decoder that will decode the `"properties"` object of each feature. This assumes (as the RFC specifies) that the coordinates are WSG84 datums in Longitude, Latitude order. Unlike the RFC, it does not allow a third coordinate for elevation/altitude.
-}
decoder : Decoder a -> Decoder (FeatureCollection WGS84 a)
decoder propertiesDecoder =
    strictDecoder Coordinates.wgs84Decoder (Decode.field "properties" propertiesDecoder)


type DecoderMode
    = Strict
    | Normalizing


{-| This decoder allows you to specify more granular control of how the decoding process should behave.

  - `decoderMode` can either be `Strict` (which is what the simpler `decoder` uses), which validates the integrity of the GeoJSON structure and will fail decoding if it doesn't conform to the model represented here. For example, the spec allows a root object in a GeoJSON document to be a FeatureCollection or a Feature. The Strict mode will fail if it isn't a FeatureCollection. The `Normalizing` mode will accept such GeoJSON and attempt to turn it into something sensible. I recommend using `Strict` for GeoJSON you control and `Normalizing` for GeoJSON you get from parties that you don't.

  - `coordinateDecoder` is a decoder for the coordinates in each geometry. You can use `Coordinates.wgs84Decoder` if you want standard WGS84 longitude/latitude pairs. However if you need altitude or want a different CRS, you can create your own decoder for these.

  - `featureDecoder` is for decoding properties for your features. However, it is given the whole feature object rather than just the `"properties"` object, since in many situations important information is stored directly on the feature object and not in the properties.

-}
advancedDecoder :
    { featureDecoder : Decoder a
    , coordinateDecoder : Decoder coord
    , decoderMode : DecoderMode
    }
    -> Decoder (FeatureCollection coord a)
advancedDecoder { featureDecoder, coordinateDecoder, decoderMode } =
    case decoderMode of
        Strict ->
            strictDecoder coordinateDecoder featureDecoder

        Normalizing ->
            normalizingDecoder coordinateDecoder featureDecoder


normalizingDecoder : Decoder coordinates -> Decoder a -> Decoder (FeatureCollection coordinates a)
normalizingDecoder coordDecoder propDecoder =
    Decode.oneOf
        [ Decode.field "features" <| Decode.list (strictFeatureDecoder coordDecoder propDecoder)
        , Decode.map List.singleton <| strictFeatureDecoder coordDecoder propDecoder
        ]


strictDecoder : Decoder coordinates -> Decoder a -> Decoder (FeatureCollection coordinates a)
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


{-| Encodes a FeatureCollection into GeoJSON. You provide a way to encode individual properties, these will be stuck into the `"properties"` object.
-}
encode : (a -> Value) -> FeatureCollection WGS84 a -> Value
encode encodeProperties =
    advancedEncode { encodeProperties = encodeProperties, encodeFeatureAttributes = always [], encodeCoordinates = Coordinates.wgs84Encode }


{-| More customized encoding which is useful for some use cases:

  - `encodeProperties` should encode whatever goes into the `"properties"` object.
  - `encodeFeatureAttributes` returns key, value pairs that will be added to the object itself.
  - `encodeCoordinates` is useful when using custom coordinates. Otherwise just use `Coordinates.wgs84Encode`.

-}
advancedEncode :
    { encodeProperties : props -> Value
    , encodeFeatureAttributes : props -> List ( String, Value )
    , encodeCoordinates : coord -> Value
    }
    -> FeatureCollection coord props
    -> Value
advancedEncode { encodeProperties, encodeFeatureAttributes, encodeCoordinates } featureCollection =
    Encode.object
        [ ( "type", Encode.string "FeatureCollection" )
        , ( "features", Encode.list (encodeItem encodeProperties encodeFeatureAttributes encodeCoordinates) featureCollection )
        ]


encodeItem : (a -> Value) -> (a -> List ( String, Value )) -> (coord -> Value) -> Feature coord a -> Value
encodeItem propsEncode foreignEncode encodePosition feature =
    foreignEncode (Feature.properties feature)
        ++ [ ( "type", Encode.string "Feature" )
           , ( "properties", propsEncode (Feature.properties feature) )
           , ( "geometry", encodeGeometries encodePosition feature )
           ]
        |> Encode.object


encodeMultiOrSingle : String -> (a -> Value) -> List a -> Value
encodeMultiOrSingle tipe encoder list =
    case list of
        [ x ] ->
            Encode.object
                [ ( "type", Encode.string tipe )
                , ( "coordinates", encoder x )
                ]

        _ ->
            Encode.object
                [ ( "type", Encode.string ("Multi" ++ tipe) )
                , ( "coordinates", Encode.list encoder list )
                ]


encodeGeometries : (coord -> Value) -> Feature coord a -> Value
encodeGeometries encodePosition feature =
    case feature of
        Points points _ ->
            encodeMultiOrSingle "Point" (\(Point position) -> encodePosition position) points

        LineStrings lineStrings _ ->
            encodeMultiOrSingle "LineString" (\(LineString a b rest) -> Encode.list encodePosition (a :: b :: rest)) lineStrings

        Polygons polygons _ ->
            encodeMultiOrSingle "Polygon" (\(Polygon outer holes) -> Encode.list identity (encodeLinearRing encodePosition outer :: List.map (encodeLinearRing encodePosition) holes)) polygons


encodeLinearRing : (coord -> Value) -> LinearRing coord -> Value
encodeLinearRing encodePosition (LinearRing a b c rest) =
    Encode.list encodePosition (a :: b :: c :: rest ++ [ a ])
