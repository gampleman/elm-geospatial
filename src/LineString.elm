module LineString exposing (LineString(..), length)

import Coordinates exposing (WGS84)
import Length exposing (Length)
import Point exposing (Point(..))
import Quantity


type LineString coordinate
    = LineString coordinate coordinate (List coordinate)


segmentFold : (( coord, coord ) -> a -> a) -> a -> LineString coord -> a
segmentFold f initial (LineString fst snd rest) =
    let
        init =
            f ( fst, snd ) initial
    in
    List.map2 Tuple.pair (snd :: rest) rest
        |> List.foldl f init


length : LineString WGS84 -> Length
length =
    segmentFold (\( coord1, coord2 ) sum -> Quantity.plus sum (Point.distance (Point coord1) (Point coord2))) Quantity.zero
