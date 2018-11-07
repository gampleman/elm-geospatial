module GeoItem exposing (GeoItem(..), properties)

import GeoPosition exposing (GeoPosition)
import LineString exposing (LineString)
import Point exposing (Point)
import Polygon exposing (Polygon)


type GeoItem a
    = Points (List Point) a
    | LineStrings (List LineString) a
    | Polygons (List Polygon) a


properties : GeoItem a -> a
properties geoItem =
    case geoItem of
        Points _ a ->
            a

        LineStrings _ a ->
            a

        Polygons _ a ->
            a
