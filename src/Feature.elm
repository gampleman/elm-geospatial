module Feature exposing (Feature(..), bbox, containsPoint, polygons, properties)

import BBox exposing (BBox)
import Coordinates exposing (WGS84)
import LineString exposing (LineString)
import Polygon exposing (Polygon)


type Feature coordinates a
    = Points (List coordinates) a
    | LineStrings (List (LineString coordinates)) a
    | Polygons (List (Polygon coordinates)) a


polygons : Feature coordinates a -> List (Polygon coordinates)
polygons feature =
    case feature of
        Polygons polys _ ->
            polys

        _ ->
            []


properties : Feature coordinates a -> a
properties feature =
    case feature of
        Points _ a ->
            a

        LineStrings _ a ->
            a

        Polygons _ a ->
            a


bbox : Feature WGS84 a -> BBox
bbox feature =
    List.foldr BBox.union BBox.empty <|
        case feature of
            Points points _ ->
                List.map (\point -> BBox.fromCoordinates { southWest = point, northEast = point }) points

            LineStrings lineStrings _ ->
                List.map LineString.bbox lineStrings

            Polygons polys _ ->
                List.map Polygon.bbox polys


containsPoint : WGS84 -> Feature WGS84 a -> Bool
containsPoint point feature =
    case feature of
        Points points _ ->
            List.any (Coordinates.equalWithPrecision 6 point) points

        LineStrings lineStrings _ ->
            List.any (LineString.containsPoint point) lineStrings

        Polygons polys _ ->
            List.any (Polygon.containsPoint point) polys
