module Feature exposing (Feature(..), properties)

import LineString exposing (LineString)
import Point exposing (Point)
import Polygon exposing (Polygon)


type Feature coordinates a
    = Points (List (Point coordinates)) a
    | LineStrings (List (LineString coordinates)) a
    | Polygons (List (Polygon coordinates)) a


properties : Feature coordinates a -> a
properties feature =
    case feature of
        Points _ a ->
            a

        LineStrings _ a ->
            a

        Polygons _ a ->
            a
