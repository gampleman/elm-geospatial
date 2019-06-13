module Map.Source exposing (featureCollection)

import Coordinates exposing (WGS84)
import FeatureCollection exposing (FeatureCollection)
import Json.Decode exposing (Value)
import Mapbox.Source exposing (GeoJSONSource, Source, SourceOption)


featureCollection : String -> List (SourceOption GeoJSONSource) -> (a -> Value) -> FeatureCollection WGS84 a -> Source
featureCollection id options encode collection =
    collection
        |> FeatureCollection.encode encode
        |> Mapbox.Source.geoJSONFromValue id options
