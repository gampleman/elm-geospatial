module PolygonTest exposing (areaTest)

import Area
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Polygon exposing (LinearRing(..), Polygon(..))
import Test exposing (..)


force : Maybe a -> a
force aMaybe =
    case aMaybe of
        Just a ->
            a

        Nothing ->
            Debug.todo "Fix this test"


areaTest : Test
areaTest =
    describe "area"
        [ test "computes the area of a simple polygon correctly" <|
            \() ->
                let
                    polygon =
                        Polygon.new
                            [ { lng = 125, lat = -15 }
                            , { lng = 113, lat = -22 }
                            , { lng = 117, lat = -37 }
                            , { lng = 130, lat = -33 }
                            , { lng = 148, lat = -39 }
                            , { lng = 154, lat = -27 }
                            , { lng = 144, lat = -15 }
                            , { lng = 125, lat = -15 }
                            ]
                in
                polygon
                    |> force
                    |> Polygon.area
                    |> Area.inSquareKilometers
                    |> Expect.within (Absolute 0.1) 7766240.9
        ]
