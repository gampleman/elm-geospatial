module Point exposing (Point(..), bearing, destination)

import Angle exposing (Angle)
import GeoPosition exposing (GeoPosition)
import Helpers exposing (degreesToAngle, lengthToAngle)
import Length exposing (Length)
import Quantity exposing (Quantity(..))


type Point
    = Point GeoPosition


{-| Finds the geographic bearing between two Points, i.e. the angle measured in degrees from the north line (0 degrees, positive clockwise).

    point1 : Point
    point1 =
        Point { lng = -75.343, lat =39.984 }

    point2 : Point
    point2 =
        Point { lng = -75.534, lat = 39.123 }

    bearing point1 point2  --> Angle.degrees -170

-}
bearing : Point -> Point -> Angle
bearing (Point start) (Point end) =
    let
        lon1 =
            degreesToAngle start.lng

        lon2 =
            degreesToAngle end.lng

        lat1 =
            degreesToAngle start.lat

        lat2 =
            degreesToAngle end.lat

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


destination : Angle -> Length -> Point -> Point
destination bearingRad dist (Point origin) =
    let
        lon1 =
            degreesToAngle origin.lng

        lat1 =
            degreesToAngle origin.lat

        radians =
            lengthToAngle dist

        lat2 =
            asin (Angle.sin lat1 * Angle.cos radians + Angle.cos lat1 * Angle.sin radians * Angle.cos bearingRad)
                |> Angle.radians

        lon2 =
            Quantity.plus lon1 <| Angle.radians <| atan2 (Angle.sin bearingRad * Angle.sin radians * Angle.cos lat1) (Angle.cos radians - Angle.sin lat1 * Angle.sin lat2)
    in
    Point { lng = Angle.inDegrees lon2, lat = Angle.inDegrees lat2 }
