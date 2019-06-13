module LineString exposing (LineString(..), bbox, containsPoint, length)

import BBox exposing (BBox)
import Coordinates exposing (WGS84)
import Helpers exposing (pointMax, pointMin)
import Length exposing (Length)
import Point
import Quantity exposing (Quantity)


type LineString coordinate
    = LineString coordinate coordinate (List coordinate)


segmentFold : (coord -> coord -> a -> a) -> a -> LineString coord -> a
segmentFold f =
    segmentWise (\a b c -> Continue (f a b c))


segmentAny : (coord -> coord -> Bool) -> LineString coord -> Bool
segmentAny f =
    segmentWise
        (\a b _ ->
            if f a b then
                Stop True

            else
                Continue False
        )
        False


type IterationControl a b
    = Continue a
    | Stop b


segmentWise : (coord -> coord -> a -> IterationControl a a) -> a -> LineString coord -> a
segmentWise f initial (LineString fst snd rest) =
    segmentWiseHelp f initial fst (snd :: rest)


segmentWiseHelp f summary prev coords =
    case coords of
        [] ->
            summary

        x :: xs ->
            case f prev x summary of
                Continue val ->
                    segmentWiseHelp f val x xs

                Stop val ->
                    val


{-| Computes the length of a line string.
-}
length : LineString WGS84 -> Length
length =
    segmentFold (\coord1 coord2 sum -> Quantity.plus sum (Point.distance coord1 coord2)) Quantity.zero


bbox : LineString WGS84 -> BBox
bbox (LineString a b cs) =
    BBox.fromCoordinates <|
        List.foldr (\c d -> { southWest = pointMin c d.southWest, northEast = pointMax c d.northEast })
            { southWest = pointMin a b, northEast = pointMax a b }
            cs


isBetweenInclusive : ( Quantity number units, Quantity number units ) -> Quantity number units -> Bool
isBetweenInclusive ( lower, upper ) value =
    (lower |> Quantity.lessThanOrEqualTo value) && (value |> Quantity.lessThanOrEqualTo upper)



-- TODO: Proper tests


containsPoint : WGS84 -> LineString WGS84 -> Bool
containsPoint point =
    segmentAny
        (\a b ->
            let
                x =
                    point.lng

                y =
                    point.lat

                x1 =
                    a.lng

                y1 =
                    a.lat

                x2 =
                    b.lng

                y2 =
                    b.lat

                dxc =
                    x |> Quantity.minus x1

                dyc =
                    y |> Quantity.minus y1

                dxl =
                    x2 |> Quantity.minus x1

                dyl =
                    y2 |> Quantity.minus y1

                cross =
                    Quantity.times dxc dyl |> Quantity.minus (Quantity.times dyc dxl)
            in
            if cross == Quantity.zero then
                if Quantity.abs dxl |> Quantity.greaterThanOrEqualTo (Quantity.abs dyl) then
                    if dxl |> Quantity.greaterThan Quantity.zero then
                        x |> isBetweenInclusive ( x1, x2 )

                    else
                        x |> isBetweenInclusive ( x2, x1 )

                else if dyl |> Quantity.greaterThan Quantity.zero then
                    y |> isBetweenInclusive ( y1, y2 )

                else
                    y |> isBetweenInclusive ( y2, y1 )

            else
                False
        )
