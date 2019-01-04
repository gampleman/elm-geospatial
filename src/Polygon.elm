module Polygon exposing (LinearRing(..), Polygon(..), area, equal, fromBBox, new)

import Angle
import Area exposing (Area)
import BBox exposing (BBox)
import Coordinates exposing (WGS84)
import Quantity exposing (toInt)


type Polygon coordinate
    = Polygon (LinearRing coordinate) (List (LinearRing coordinate))


type LinearRing coordinate
    = LinearRing coordinate coordinate coordinate (List coordinate)


{-| Creates a rectangular polygon from a BBox.
-}
fromBBox : BBox -> Polygon WGS84
fromBBox bBox =
    let
        { southWest, northEast } =
            BBox.coordinates bBox
    in
    Polygon (LinearRing southWest { lng = southWest.lng, lat = northEast.lat } northEast [ { lng = northEast.lng, lat = southWest.lat } ]) []


new : List { lng : Float, lat : Float } -> Maybe (Polygon WGS84)
new coords =
    let
        toWSG { lng, lat } =
            { lng = Angle.degrees lng, lat = Angle.degrees lat }
    in
    case coords of
        a :: b :: c :: rest ->
            Just (Polygon (LinearRing (toWSG a) (toWSG b) (toWSG c) (List.map toWSG rest)) [])

        _ ->
            Nothing


{-| While nothing prevents you from using normal elm equality (`==`), it may be too strict sometimes.

Polygons consist of rings, and so while order within the ring matters, where you start does not. Furthermore, the direction of the ring usually doesn't matter (although some software does care about this).

This equality function will make sure to align the rings properly before comparing them. Optionally you can instruct it to ingore the direction of the rings or only consider a number of significant decimal digits.

    1  a -- b     2  b -- c    3  a -- d
       |    |        |    |       |    |
       d -- c        a -- d       b -- c

    1: [(0, 0), (1, 0), (1, 1), (0, 1)]
    2: [(0, 1), (0, 0), (1, 0), (1, 1)]
    3: [(0, 0), (0, 1), (1, 1), (1, 0)]

    In this figure the three polygons are identical squares,
    but (1) and (2) differ in their starting point, whereas
    (1) and (3) differ in their direction. This function
    would consider (1) and (2) equal, but (1) and (3) only
    if you passed ignoreDirection = True.

-}
equal : { precision : Int, ignoreDirection : Bool } -> Polygon WGS84 -> Polygon WGS84 -> Bool
equal options (Polygon aOuter aHoles) (Polygon bOuter bHoles) =
    linearRingEqual options aOuter bOuter && List.all identity (List.map2 (linearRingEqual options) aHoles bHoles)


rotateToMatchStart : Int -> LinearRing WGS84 -> WGS84 -> Maybe (LinearRing WGS84)
rotateToMatchStart tolerance ((LinearRing _ _ _ rest) as a) coord =
    let
        helper steps (LinearRing a1 a2 a3 a4s) =
            if steps == 0 then
                Nothing

            else if Coordinates.equalWithPrecision tolerance a1 coord then
                Just (LinearRing a1 a2 a3 a4s)

            else
                case a4s of
                    [] ->
                        helper (steps - 1) (LinearRing a2 a3 a1 [])

                    h :: t ->
                        helper (steps - 1) (LinearRing a2 a3 h (t ++ [ a1 ]))
    in
    helper (List.length rest + 3) a


linearRingEqual : { precision : Int, ignoreDirection : Bool } -> LinearRing WGS84 -> LinearRing WGS84 -> Bool
linearRingEqual { precision, ignoreDirection } ((LinearRing a1 a2 a3 a4s) as a) ((LinearRing c1 c2 c3 c4s) as c) =
    if List.length a4s /= List.length c4s then
        False

    else
        case rotateToMatchStart precision c a1 of
            Just ((LinearRing b1 b2 b3 b4s) as b) ->
                if Coordinates.equalWithPrecision precision a2 b2 then
                    Coordinates.equalWithPrecision precision a3 b3
                        && List.all identity (List.map2 (Coordinates.equalWithPrecision precision) a4s b4s)

                else if ignoreDirection then
                    -- we'll attept to reverse the second ring
                    List.all identity
                        (List.map2 (Coordinates.equalWithPrecision precision)
                            (a1 :: a2 :: a3 :: a4s)
                            (b1 :: List.reverse b4s ++ [ b3, b2 ])
                        )

                else
                    False

            Nothing ->
                False


{-| Computes the area of the polygon.
-}
area : Polygon WGS84 -> Area
area (Polygon outside holes) =
    Quantity.abs (ringArea outside) |> Quantity.minus (Quantity.abs <| Quantity.sum <| List.map ringArea holes)


ringArea : LinearRing WGS84 -> Area
ringArea (LinearRing a b c rest) =
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
    helper 0 (a :: b :: c :: rest ++ [ a ]) * radius * radius / 2 |> Area.squareMeters
