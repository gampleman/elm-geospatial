module Helpers exposing (angleToLength, degreesToAngle, lengthToAngle, pointMax, pointMin, pointToPoint2d)

import Angle exposing (Angle)
import Length exposing (Length)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)


earthRadius : Float
earthRadius =
    6371.0088


lengthPerAngle =
    Length.kilometers earthRadius |> Quantity.per (Angle.radians 1)


lengthToAngle : Length -> Angle
lengthToAngle =
    Quantity.at_ lengthPerAngle


angleToLength : Angle -> Length
angleToLength =
    Quantity.at lengthPerAngle


mod : Float -> Float -> Float
mod a b =
    let
        frac =
            a / b
    in
    (frac - toFloat (truncate frac)) * b


degreesToAngle : Float -> Angle
degreesToAngle degrees =
    Angle.degrees (mod degrees 360)


pointMax : { lng : Angle, lat : Angle } -> { lng : Angle, lat : Angle } -> { lng : Angle, lat : Angle }
pointMax a b =
    { lng = Quantity.max a.lng b.lng, lat = Quantity.max a.lat b.lat }


pointMin : { lng : Angle, lat : Angle } -> { lng : Angle, lat : Angle } -> { lng : Angle, lat : Angle }
pointMin a b =
    { lng = Quantity.min a.lng b.lng, lat = Quantity.min a.lat b.lat }


pointToPoint2d : { lng : Angle, lat : Angle } -> Point2d
pointToPoint2d { lng, lat } =
    Point2d.fromCoordinates ( Angle.inDegrees lng, Angle.inDegrees lat )
