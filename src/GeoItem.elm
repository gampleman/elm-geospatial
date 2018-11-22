module GeoItem exposing (GeoItem(..), properties)

import LineString exposing (LineString)
import Point exposing (Point)
import Polygon exposing (Polygon)


type GeoItem coordinates a
    = Points (List (Point coordinates)) a
    | LineStrings (List (LineString coordinates)) a
    | Polygons (List (Polygon coordinates)) a


properties : GeoItem coordinates a -> a
properties geoItem =
    case geoItem of
        Points _ a ->
            a

        LineStrings _ a ->
            a

        Polygons _ a ->
            a
