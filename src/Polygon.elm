module Polygon exposing (LinearRing(..), Polygon(..), area, new)

import Angle
import Area exposing (Area)
import Coordinates exposing (WGS84)
import Quantity exposing (toInt)


type Polygon coordinate
    = Polygon (LinearRing coordinate) (List (LinearRing coordinate))


type LinearRing coordinate
    = LinearRing coordinate coordinate coordinate coordinate (List coordinate)


new : List { lng : Float, lat : Float } -> Maybe (Polygon WGS84)
new coords =
    let
        toWSG { lng, lat } =
            { lng = Angle.degrees lng, lat = Angle.degrees lat }
    in
    case coords of
        a :: b :: c :: d :: rest ->
            Just (Polygon (LinearRing (toWSG a) (toWSG b) (toWSG c) (toWSG d) (List.map toWSG rest)) [])

        _ ->
            Nothing


area : Polygon WGS84 -> Area
area (Polygon outside holes) =
    Quantity.abs (ringArea outside) |> Quantity.minus (Quantity.abs <| Quantity.sum <| List.map ringArea holes)


ringArea : LinearRing WGS84 -> Area
ringArea (LinearRing a b c d rest) =
    let
        radius =
            6378137

        helper total list =
            case list of
                [ y, z ] ->
                    total + (Angle.inRadians a.lng - Angle.inRadians y.lng) * Angle.sin z.lat + (Angle.inRadians b.lng - Angle.inRadians z.lng) * Angle.sin a.lat

                p1 :: p2 :: p3 :: tail ->
                    helper (total + (Angle.inRadians p3.lng - Angle.inRadians p1.lng) * Angle.sin p2.lat) (p2 :: p3 :: tail)

                _ ->
                    -- crash
                    helper total list
    in
    helper 0 (a :: b :: c :: d :: rest) * radius * radius / 2 |> Area.squareMeters
