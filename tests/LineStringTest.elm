module LineStringTest exposing (lengthTest)

import Angle
import Coordinates exposing (WGS84)
import Expect exposing (FloatingPointTolerance(..))
import Length
import LineString exposing (LineString(..))
import Test exposing (Test, describe, test)


coord : Float -> Float -> WGS84
coord lng lat =
    { lat = Angle.degrees lat, lng = Angle.degrees lng }


lengthTest : Test
lengthTest =
    describe "length"
        [ test "computes the correct length" <|
            \() ->
                LineString
                    (coord 115 -32)
                    (coord 131 -22)
                    [ coord 143 -25
                    , coord 150 -34
                    ]
                    |> LineString.length
                    |> Length.inMiles
                    |> Expect.within (Absolute 0.001) 2738.966
        ]
