module Polygon exposing (LinearRing(..), Polygon(..), area)

import Area exposing (Area)
import GeoPosition exposing (GeoPosition)
import Quantity


type Polygon
    = Polygon LinearRing (List LinearRing)


type LinearRing
    = LinearRing GeoPosition GeoPosition GeoPosition GeoPosition (List GeoPosition)


area : Polygon -> Area
area (Polygon outside holes) =
    Quantity.abs (ringArea outside) |> Quantity.minus (Quantity.abs <| Quantity.sum <| List.map ringArea holes)


ringArea : LinearRing -> Area
ringArea (LinearRing a b c d rest) =
    let
        rad n =
            n * pi / 180

        radius =
            6378137

        helper total list =
            case list of
                [ y, z ] ->
                    total + (rad a.lng - rad y.lng) * sin (rad z.lat) + (rad b.lng - rad z.lng) * sin (rad a.lat)

                p1 :: p2 :: p3 :: tail ->
                    helper (total + (rad p3.lng - rad p1.lng) * sin (rad p2.lat)) (p2 :: p3 :: tail)

                _ ->
                    -- crash
                    helper total list
    in
    helper 0 (a :: b :: c :: d :: rest) * radius * radius / 2 |> Area.squareMeters
