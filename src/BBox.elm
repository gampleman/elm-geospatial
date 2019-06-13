module BBox exposing
    ( BBox, fromCoordinates, fromExtrema, coordinates, everything, empty, union, decoder, encode
    , area, containsPoint, intersection, maxLat, maxLng, minLat, minLng, overlap
    )

{-|

@docs BBox, fromCoordinates, fromExtrema, coordinates, everything, empty, union, decoder, encode

-}

import Angle exposing (Angle)
import Area exposing (Area)
import Coordinates exposing (WGS84)
import Helpers exposing (angleToLength)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Quantity exposing (Quantity)


{-| BBox or bounding box represents a rectangle aligned with the coordinate system.
It is useful primarily to accelerate various algorithms, for example it is much faster to
check if bounding boxes of polygons overlap than to check if actual polygons overlap.
-}
type BBox
    = BBox WGS84 WGS84


{-| Get's the area of the BBox. The formula right now is probably wrong, not taking earth curvature into account properly. But good enough for small bboxes.
-}
area : BBox -> Area
area (BBox southWest northEast) =
    Quantity.times
        (angleToLength (northEast.lat |> Quantity.minus southWest.lat))
        (angleToLength (northEast.lng |> Quantity.minus southWest.lng))


union : BBox -> BBox -> BBox
union (BBox asw ane) (BBox bsw bne) =
    BBox { lng = Quantity.min asw.lng bsw.lng, lat = Quantity.min asw.lat bsw.lat }
        { lng = Quantity.max ane.lng bne.lng, lat = Quantity.max ane.lat bne.lat }


intersection : BBox -> BBox -> BBox
intersection (BBox asw ane) (BBox bsw bne) =
    BBox { lng = Quantity.max asw.lng bsw.lng, lat = Quantity.max asw.lat bsw.lat }
        { lng = Quantity.min ane.lng bne.lng, lat = Quantity.min ane.lat bne.lat }


overlap : BBox -> BBox -> Bool
overlap a b =
    (minLng a |> Quantity.lessThanOrEqualTo (maxLng b))
        && (maxLng a |> Quantity.greaterThanOrEqualTo (minLng b))
        && (minLat a |> Quantity.lessThanOrEqualTo (maxLat b))
        && (maxLat a |> Quantity.greaterThanOrEqualTo (minLat b))


{-| Create a BBox from a south-west and north-east corners.
-}
fromCoordinates : { southWest : WGS84, northEast : WGS84 } -> BBox
fromCoordinates inp =
    BBox inp.southWest inp.northEast


{-| This is a helper for using literals.
-}
fromExtrema : { minLng : Float, minLat : Float, maxLng : Float, maxLat : Float } -> BBox
fromExtrema extrema =
    BBox { lng = Angle.degrees extrema.minLng, lat = Angle.degrees extrema.minLat }
        { lng = Angle.degrees extrema.maxLng, lat = Angle.degrees extrema.maxLat }


{-| Retrieve the coordinates.
-}
coordinates : BBox -> { southWest : WGS84, northEast : WGS84 }
coordinates (BBox southWest northEast) =
    { southWest = southWest, northEast = northEast }


{-| This contains the whole world, at least assuming a web mercator projection.
-}
everything : BBox
everything =
    fromExtrema { minLng = -180, minLat = -85, maxLng = 180, maxLat = 85 }


minLng : BBox -> Angle
minLng (BBox southWest northEast) =
    southWest.lng


maxLng : BBox -> Angle
maxLng (BBox southWest northEast) =
    northEast.lng


minLat : BBox -> Angle
minLat (BBox southWest northEast) =
    southWest.lat


maxLat : BBox -> Angle
maxLat (BBox southWest northEast) =
    northEast.lat


{-| This is an empty bbox, in the sense that a union with it will always produce whatever was uninoned:

    BBox.union something BBox.empty == something

-}
empty : BBox
empty =
    fromExtrema { minLng = infinity, minLat = infinity, maxLng = -infinity, maxLat = -infinity }


infinity =
    1 / 0


containsPoint : WGS84 -> BBox -> Bool
containsPoint point bbox =
    (point.lat |> Quantity.greaterThanOrEqualTo (minLat bbox))
        && (point.lat |> Quantity.lessThanOrEqualTo (maxLat bbox))
        && (point.lng |> Quantity.greaterThanOrEqualTo (minLng bbox))
        && (point.lng |> Quantity.lessThanOrEqualTo (maxLng bbox))


{-| GeoJSON has a way of encoding BBoxes, you can use this decoder if you need to get them.
-}
decoder : Decoder BBox
decoder =
    Decode.andThen
        (\coords ->
            case coords of
                [ minLng_, minLat_, maxLng_, maxLat_ ] ->
                    Decode.succeed <| fromExtrema { minLng = minLng_, minLat = minLat_, maxLng = maxLng_, maxLat = maxLat_ }

                _ ->
                    Decode.fail "Wrong number of coordinates for bbox value"
        )
        (Decode.list Decode.float)


{-| This is a standard GeoJSON encoding. You would typically put this in a `"bbox"` field on a feature.
-}
encode : BBox -> Value
encode (BBox southWest northEast) =
    Encode.list (Angle.inDegrees >> Encode.float) [ southWest.lng, southWest.lat, northEast.lng, northEast.lat ]
