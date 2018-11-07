module PointTest exposing (bearingTest, destinationTest)

import Angle
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Length
import Point exposing (Point(..))
import Test exposing (..)


bearingTest : Test
bearingTest =
    test "bearing computes the correct bearing" <|
        \() ->
            let
                start =
                    Point { lng = -75, lat = 45 }

                end =
                    Point { lng = 20, lat = 60 }
            in
            Point.bearing start end
                |> Angle.inDegrees
                |> Expect.within (Absolute 0.01) 37.75


expectEqualToPoint : FloatingPointTolerance -> Point -> Point -> Expectation
expectEqualToPoint tolerance (Point { lat, lng }) =
    Expect.all
        [ \(Point actual) -> Expect.within tolerance lat actual.lat
        , \(Point actual) -> Expect.within tolerance lng actual.lng
        ]


destinationTest : Test
destinationTest =
    describe "destination"
        (let
            point =
                Point { lng = -75, lat = 39 }
         in
         [ test "bearing 0" <|
            \() ->
                Point.destination (Angle.degrees 0) (Length.kilometers 100) point
                    |> expectEqualToPoint (Absolute 0.001) (Point { lng = -75, lat = 39.899 })
         , test "bearing 180" <|
            \() ->
                Point.destination (Angle.degrees 180) (Length.kilometers 100) point
                    |> expectEqualToPoint (Absolute 0.001) (Point { lng = -75, lat = 38.1 })
         , test "bearing 90" <|
            \() ->
                Point.destination (Angle.degrees 90) (Length.kilometers 100) point
                    |> expectEqualToPoint (Absolute 0.001) (Point { lng = -73.842853, lat = 38.994285 })
         , test "far away" <|
            \() ->
                Point.destination (Angle.degrees 90) (Length.kilometers 5000) point
                    |> expectEqualToPoint (Absolute 0.001) (Point { lng = -22.885356, lat = 26.440011 })
         ]
        )
