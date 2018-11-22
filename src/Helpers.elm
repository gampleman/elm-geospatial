module Helpers exposing (angleToLength, degreesToAngle, lengthToAngle)

import Angle exposing (Angle)
import Length exposing (Length)
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
