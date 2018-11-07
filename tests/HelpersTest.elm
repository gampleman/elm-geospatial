module HelpersTest exposing (testDegreesToAngle)

import Angle
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (floatRange)
import Helpers exposing (degreesToAngle)
import Test exposing (Test, describe, fuzz, test)


testDegreesToAngle : Test
testDegreesToAngle =
    describe "degreesToAngle"
        [ test "degrees conversion 60" <|
            \() ->
                degreesToAngle 60
                    |> Angle.inRadians
                    |> Expect.within (Absolute 0.0001) (pi / 3)
        , test "degrees conversion 270" <|
            \() ->
                degreesToAngle 270
                    |> Angle.inRadians
                    |> Expect.within (Absolute 0.0001) (pi * 1.5)
        , test "degrees conversion -180" <|
            \() ->
                degreesToAngle -180
                    |> Angle.inRadians
                    |> Expect.within (Absolute 0.0001) -pi
        ]
