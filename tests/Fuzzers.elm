module Fuzzers exposing (featureCollectionFuzzer, featureFuzzer)

import Angle
import Coordinates exposing (WGS84)
import Feature exposing (Feature(..))
import FeatureCollection exposing (FeatureCollection)
import Fuzz exposing (Fuzzer, list)
import LineString exposing (LineString(..))
import Point
import Polygon exposing (LinearRing(..), Polygon(..))


featureCollectionFuzzer : Fuzzer a -> Fuzzer (FeatureCollection WGS84 a)
featureCollectionFuzzer =
    list << featureFuzzer


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


featureFuzzer : Fuzzer a -> Fuzzer (Feature WGS84 a)
featureFuzzer props =
    Fuzz.oneOf
        [ Fuzz.map2 Points (shortNonEmptyList wsg84Fuzzer) props
        , Fuzz.map2 LineStrings (shortNonEmptyList lineStringFuzzer) props
        , Fuzz.map2 Polygons (shortNonEmptyList polygonFuzzer) props
        ]


wsg84Fuzzer : Fuzzer WGS84
wsg84Fuzzer =
    Fuzz.map2 WGS84 (Fuzz.map Angle.degrees (Fuzz.floatRange -90 90)) (Fuzz.map Angle.degrees (Fuzz.floatRange -180 180))


lineStringFuzzer : Fuzzer (LineString WGS84)
lineStringFuzzer =
    Fuzz.map3 LineString wsg84Fuzzer wsg84Fuzzer (shortList wsg84Fuzzer)


polygonFuzzer : Fuzzer (Polygon WGS84)
polygonFuzzer =
    Fuzz.map2 Polygon linearRingFuzzer (shortList linearRingFuzzer)


linearRingFuzzer : Fuzzer (LinearRing WGS84)
linearRingFuzzer =
    Fuzz.map4 LinearRing wsg84Fuzzer wsg84Fuzzer wsg84Fuzzer (shortList wsg84Fuzzer)
