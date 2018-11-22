module Point exposing (Point(..), bearing, destination, distance, new)

import Angle exposing (Angle)
import Coordinates exposing (WGS84)
import Helpers exposing (angleToLength, degreesToAngle, lengthToAngle)
import Length exposing (Length)
import Quantity exposing (Quantity(..))


type Point coordinate
    = Point coordinate


new : { lng : Float, lat : Float } -> Point WGS84
new { lng, lat } =
    Point { lng = Angle.degrees lng, lat = Angle.degrees lat }


asAngles (Point { lng, lat }) =
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
bearing : Point WGS84 -> Point WGS84 -> Angle
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


destination : Angle -> Length -> Point WGS84 -> Point WGS84
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
    Point { lng = lon2, lat = lat2 }


distance : Point WGS84 -> Point WGS84 -> Length
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
