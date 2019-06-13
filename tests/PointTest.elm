module PointTest exposing (bearingTest, destinationTest, distanceTest, voronoiTest)

import Angle
import BBox
import Coordinates exposing (WGS84)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Length
import Point
import Polygon
import Test exposing (..)
import TestHelpers exposing (equalWithinTolerance, listsEquivalent, multiplePolygonsEqual, polygonsEqual)


bearingTest : Test
bearingTest =
    test "bearing computes the correct bearing" <|
        \() ->
            let
                start =
                    Point.new { lng = -75, lat = 45 }

                end =
                    Point.new { lng = 20, lat = 60 }
            in
            Point.bearing start end
                |> Angle.inDegrees
                |> Expect.within (Absolute 0.01) 37.75


expectEqualToPoint : FloatingPointTolerance -> WGS84 -> WGS84 -> Expectation
expectEqualToPoint tolerance { lat, lng } =
    Expect.all
        [ \actual -> Expect.within tolerance (Angle.inDegrees lat) (Angle.inDegrees actual.lat)
        , \actual -> Expect.within tolerance (Angle.inDegrees lng) (Angle.inDegrees actual.lng)
        ]


destinationTest : Test
destinationTest =
    describe "destination"
        (let
            point =
                Point.new { lng = -75, lat = 39 }
         in
         [ test "bearing 0" <|
            \() ->
                Point.destination (Angle.degrees 0) (Length.kilometers 100) point
                    |> expectEqualToPoint (Absolute 0.001) (Point.new { lng = -75, lat = 39.899 })
         , test "bearing 180" <|
            \() ->
                Point.destination (Angle.degrees 180) (Length.kilometers 100) point
                    |> expectEqualToPoint (Absolute 0.001) (Point.new { lng = -75, lat = 38.1 })
         , test "bearing 90" <|
            \() ->
                Point.destination (Angle.degrees 90) (Length.kilometers 100) point
                    |> expectEqualToPoint (Absolute 0.001) (Point.new { lng = -73.842853, lat = 38.994285 })
         , test "far away" <|
            \() ->
                Point.destination (Angle.degrees 90) (Length.kilometers 5000) point
                    |> expectEqualToPoint (Absolute 0.001) (Point.new { lng = -22.885356, lat = 26.440011 })
         ]
        )


distanceTest : Test
distanceTest =
    describe "distance"
        [ test "computes the proper distance between two points" <|
            \() ->
                Point.distance (Point.new { lng = -75.343, lat = 39.984 }) (Point.new { lng = -75.534, lat = 39.123 })
                    |> Length.inKilometers
                    |> Expect.within (Absolute 0.0001) 97.12922118967835
        ]


voronoiTest : Test
voronoiTest =
    describe "voronoi"
        [ test "computes simple case of voronoi" <|
            \() ->
                Point.voronoi (BBox.fromExtrema { minLng = 140, minLat = -40, maxLng = 160, maxLat = -30 }) [ Point.new { lng = 145, lat = -37 } ]
                    |> List.filterMap identity
                    |> multiplePolygonsEqual { precision = 2, ignoreDirection = True }
                        (Polygon.new
                            [ { lng = 140, lat = -40 }
                            , { lng = 140, lat = -30 }
                            , { lng = 160, lat = -30 }
                            , { lng = 160, lat = -40 }
                            ]
                            |> List.singleton
                            |> List.filterMap identity
                        )
        , test "computes more complex voronoi" <|
            \() ->
                let
                    points =
                        List.map Point.new
                            [ { lng = 144.33837890625, lat = -37.14280344371683 }
                            , { lng = 144.931640625, lat = -37.35269280367274 }
                            , { lng = 145.140380859375, lat = -36.456636011596196 }
                            , { lng = 145.469970703125, lat = -36.77409249464194 }
                            , { lng = 145.755615234375, lat = -37.090239803072066 }
                            , { lng = 145.4150390625, lat = -37.52715361723378 }
                            , { lng = 145.887451171875, lat = -37.483576550426996 }
                            , { lng = 144.60205078125, lat = -36.57142382346275 }
                            , { lng = 144.86572265625, lat = -37.596824001083654 }
                            ]

                    polygons =
                        List.filterMap Polygon.new
                            [ [ { lng = 143, lat = -36.17865921750102 }
                              , { lng = 144.7306872649525, lat = -36.97731260260012 }
                              , { lng = 144.58469067823404, lat = -37.38997746052463 }
                              , { lng = 144.05948693381836, lat = -38 }
                              , { lng = 143, lat = -38 }
                              ]
                            , [ { lng = 144.58469067823404, lat = -37.38997746052463 }
                              , { lng = 144.7306872649525, lat = -36.97731260260012 }
                              , { lng = 144.9502509842771, lat = -36.88468636726737 }
                              , { lng = 145.02826439034735, lat = -36.902859896834244 }
                              , { lng = 145.28197625539775, lat = -37.13891325365974 }
                              , { lng = 145.13749583645568, lat = -37.53924080856172 }
                              ]
                            , [ { lng = 146, lat = -35.89398200640089 }
                              , { lng = 145.02826439034735, lat = -36.902859896834244 }
                              , { lng = 144.9502509842771, lat = -36.88468636726737 }
                              , { lng = 144.54837968093918, lat = -35 }
                              , { lng = 146, lat = -35 }
                              ]
                            , [ { lng = 145.02826439034735, lat = -36.902859896834244 }
                              , { lng = 146, lat = -35.89398200640089 }
                              , { lng = 146, lat = -36.58231923105632 }
                              , { lng = 145.37634807465002, lat = -37.145797192532186 }
                              , { lng = 145.28197625539775, lat = -37.13891325365974 }
                              ]
                            , [ { lng = 145.37634807465002, lat = -37.145797192532186 }
                              , { lng = 146, lat = -36.58231923105632 }
                              , { lng = 146, lat = -37.22709115402949 }
                              , { lng = 145.63680507441603, lat = -37.348824340099675 }
                              ]
                            , [ { lng = 145.13749583645568, lat = -37.53924080856172 }
                              , { lng = 145.28197625539775, lat = -37.13891325365974 }
                              , { lng = 145.37634807465002, lat = -37.145797192532186 }
                              , { lng = 145.63680507441603, lat = -37.348824340099675 }
                              , { lng = 145.69687188417421, lat = -38 }
                              , { lng = 145.1959341358453, lat = -38 }
                              ]
                            , [ { lng = 145.63680507441603, lat = -37.348824340099675 }
                              , { lng = 146, lat = -37.22709115402949 }
                              , { lng = 146, lat = -38 }
                              , { lng = 145.69687188417421, lat = -38 }
                              ]
                            , [ { lng = 144.54837968093918, lat = -35 }
                              , { lng = 144.9502509842771, lat = -36.88468636726737 }
                              , { lng = 144.7306872649525, lat = -36.97731260260012 }
                              , { lng = 143, lat = -36.17865921750102 }
                              , { lng = 143, lat = -35 }
                              ]
                            , [ { lng = 144.05948693381836, lat = -38 }
                              , { lng = 144.58469067823404, lat = -37.38997746052463 }
                              , { lng = 145.13749583645568, lat = -37.53924080856172 }
                              , { lng = 145.1959341358453, lat = -38 }
                              ]
                            ]
                in
                Point.voronoi (BBox.fromExtrema { minLng = 143, minLat = -38, maxLng = 146, maxLat = -35 }) points
                    |> List.filterMap identity
                    |> multiplePolygonsEqual { precision = 2, ignoreDirection = True } polygons
        ]
