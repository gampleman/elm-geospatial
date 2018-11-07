module Helpers exposing (degreesToAngle, lengthToAngle)

import Angle exposing (Angle)
import Length exposing (Length)


earthRadius : Float
earthRadius =
    6371.0088


lengthToAngle : Length -> Angle
lengthToAngle l =
    Angle.radians <| Length.inKilometers l / earthRadius


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
