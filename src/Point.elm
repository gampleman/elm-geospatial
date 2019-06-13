module Point exposing
    ( new
    , bearing, destination, distance
    , voronoi
    )

{-| A Point represents a single point in a coordinate space.


## Constructing

@docs new


## Measurement

@docs bearing, destination, distance


## Transformation

@docs voronoi

-}

import Angle exposing (Angle)
import Array
import BBox exposing (BBox)
import BoundingBox2d exposing (BoundingBox2d)
import Coordinates exposing (WGS84)
import Dict
import Helpers exposing (angleToLength, degreesToAngle, lengthToAngle, pointToPoint2d)
import Length exposing (Length)
import Point2d exposing (Point2d)
import Polygon exposing (Polygon)
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity(..))
import VoronoiDiagram2d exposing (VoronoiDiagram2d)


new : { lng : Float, lat : Float } -> WGS84
new { lng, lat } =
    { lng = Angle.degrees lng, lat = Angle.degrees lat }


asAngles { lng, lat } =
    ( lng, lat )


{-| Finds the geographic bearing between two Points, i.e. the angle measured in degrees from the north line (0 degrees, positive clockwise).

    point1 : Point
    point1 =
        Point { lng = -75.343, lat =39.984 }

    point2 : Point
    point2 =
        Point { lng = -75.534, lat = 39.123 }

    bearing point1 point2  --> Angle.degrees -170

-}
bearing : WGS84 -> WGS84 -> Angle
bearing start end =
    let
        ( lon1, lat1 ) =
            asAngles start

        ( lon2, lat2 ) =
            asAngles end

        a =
            Angle.sin (lon2 |> Quantity.minus lon1) * Angle.cos lat2 |> Quantity

        b =
            Angle.cos lat1
                * Angle.sin lat2
                - Angle.sin lat1
                * Angle.cos lat2
                * Angle.cos (lon2 |> Quantity.minus lon1)
                |> Quantity
    in
    Angle.atan2 a b


destination : Angle -> Length -> WGS84 -> WGS84
destination bearingRad dist origin =
    let
        ( lon1, lat1 ) =
            asAngles origin

        radians =
            lengthToAngle dist

        lat2 =
            asin (Angle.sin lat1 * Angle.cos radians + Angle.cos lat1 * Angle.sin radians * Angle.cos bearingRad)
                |> Angle.radians

        lon2 =
            Quantity.plus lon1 <| Angle.radians <| atan2 (Angle.sin bearingRad * Angle.sin radians * Angle.cos lat1) (Angle.cos radians - Angle.sin lat1 * Angle.sin lat2)
    in
    { lng = lon2, lat = lat2 }


distance : WGS84 -> WGS84 -> Length
distance from to =
    let
        ( lon1, lat1 ) =
            asAngles from

        ( lon2, lat2 ) =
            asAngles to

        dLat =
            lat2 |> Quantity.minus lat1

        dLon =
            lon2 |> Quantity.minus lon1

        powsin =
            Quantity.divideBy 2 >> Angle.sin >> (\x -> x ^ 2)

        a =
            powsin dLat + powsin dLon * Angle.cos lat1 * Angle.cos lat2
    in
    angleToLength (Angle.radians (2 * atan2 (sqrt a) (sqrt (1 - a))))


bboxToBoundingBox2d : BBox -> BoundingBox2d
bboxToBoundingBox2d bbox =
    let
        { southWest, northEast } =
            BBox.coordinates bbox
    in
    BoundingBox2d.from (pointToPoint2d southWest) (pointToPoint2d northEast)


polygon2dToPolygon : Polygon2d -> Maybe (Polygon WGS84)
polygon2dToPolygon poly =
    Polygon2d.outerLoop poly
        |> List.map (\point -> { lng = point |> Point2d.xCoordinate, lat = point |> Point2d.yCoordinate })
        |> List.reverse
        |> Polygon.new


voronoi : BBox -> List WGS84 -> List (Maybe (Polygon WGS84))
voronoi bbox points =
    case VoronoiDiagram2d.fromPoints (Array.fromList (List.map pointToPoint2d points)) of
        Ok diagram ->
            let
                dict =
                    VoronoiDiagram2d.polygons (bboxToBoundingBox2d bbox) diagram
                        |> List.filterMap (\( point, poly ) -> Maybe.map (Tuple.pair (Point2d.coordinates point)) (polygon2dToPolygon poly))
                        |> Dict.fromList
            in
            points
                |> List.map (\{ lng, lat } -> Dict.get ( Angle.inDegrees lng, Angle.inDegrees lat ) dict)

        Err _ ->
            List.map (always Nothing) points
