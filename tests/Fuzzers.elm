module Fuzzers exposing (geoCollectionFuzzer, geoItemFuzzer)

import Fuzz exposing (Fuzzer, list)
import GeoCollection exposing (GeoCollection)
import GeoItem exposing (GeoItem(..))
import GeoPosition exposing (GeoPosition)
import LineString exposing (LineString(..))
import Point exposing (Point(..))
import Polygon exposing (LinearRing(..), Polygon(..))


geoCollectionFuzzer : Fuzzer a -> Fuzzer (GeoCollection a)
geoCollectionFuzzer =
    list << geoItemFuzzer


shortNonEmptyList : Fuzzer a -> Fuzzer (List a)
shortNonEmptyList a =
    Fuzz.frequency
        [ ( 1, list a )
        , ( 20, Fuzz.map2 (\b c -> [ b, c ]) a a )
        , ( 79, Fuzz.map List.singleton a )
        ]


shortList : Fuzzer a -> Fuzzer (List a)
shortList a =
    Fuzz.frequency
        [ ( 4, list a )
        , ( 20, Fuzz.map2 (\b c -> [ b, c ]) a a )
        , ( 36, Fuzz.map List.singleton a )
        , ( 40, Fuzz.constant [] )
        ]


geoItemFuzzer : Fuzzer a -> Fuzzer (GeoItem a)
geoItemFuzzer props =
    Fuzz.oneOf
        [ Fuzz.map2 Points (shortNonEmptyList pointFuzzer) props
        , Fuzz.map2 LineStrings (shortNonEmptyList lineStringFuzzer) props
        , Fuzz.map2 Polygons (shortNonEmptyList polygonFuzzer) props
        ]


geoPositionFuzzer : Fuzzer GeoPosition
geoPositionFuzzer =
    Fuzz.map2 GeoPosition (Fuzz.floatRange -90 90) (Fuzz.floatRange -180 180)


pointFuzzer : Fuzzer Point
pointFuzzer =
    Fuzz.map Point geoPositionFuzzer


lineStringFuzzer : Fuzzer LineString
lineStringFuzzer =
    Fuzz.map3 LineString geoPositionFuzzer geoPositionFuzzer (shortList geoPositionFuzzer)


polygonFuzzer : Fuzzer Polygon
polygonFuzzer =
    Fuzz.map2 Polygon linearRingFuzzer (shortList linearRingFuzzer)


linearRingFuzzer : Fuzzer LinearRing
linearRingFuzzer =
    Fuzz.map5 LinearRing geoPositionFuzzer geoPositionFuzzer geoPositionFuzzer geoPositionFuzzer (shortList geoPositionFuzzer)
