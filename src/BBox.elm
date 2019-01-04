module BBox exposing (BBox, fromCoordinates, fromExtrema, coordinates, everything, decoder, encode)

{-|

@docs BBox, fromCoordinates, fromExtrema, coordinates, everything, decoder, encode

-}

import Angle exposing (Angle)
import Coordinates exposing (WGS84)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


{-| BBox or bounding box represents a rectangle aligned with the coordinate system.
It is useful primarily to accelerate various algorithms, for example it is much faster to
check if bounding boxes of polygons overlap than to check if actual polygons overlap.
-}
type BBox
    = BBox WGS84 WGS84


{-| Create a BBox from a south-west and north-east corners.
-}
fromCoordinates : { southWest : WGS84, northEast : WGS84 } -> BBox
fromCoordinates inp =
    BBox inp.southWest inp.northEast


{-| This is a helper for using literals.
-}
fromExtrema : { minLng : Float, minLat : Float, maxLng : Float, maxLat : Float } -> BBox
fromExtrema { minLng, minLat, maxLng, maxLat } =
    BBox { lng = Angle.degrees minLng, lat = Angle.degrees minLat }
        { lng = Angle.degrees maxLng, lat = Angle.degrees maxLat }


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


{-| GeoJSON has a way of encoding BBoxes, you can use this decoder if you need to get them.
-}
decoder : Decoder BBox
decoder =
    Decode.andThen
        (\coords ->
            case coords of
                [ minLng, minLat, maxLng, maxLat ] ->
                    Decode.succeed <| fromExtrema { minLng = minLng, minLat = minLat, maxLng = maxLng, maxLat = maxLat }

                _ ->
                    Decode.fail "Wrong number of coordinates for bbox value"
        )
        (Decode.list Decode.float)


{-| This is a standard GeoJSON encoding. You would typically put this in a `"bbox"` field on a feature.
-}
encode : BBox -> Value
encode (BBox southWest northEast) =
    Encode.list (Angle.inDegrees >> Encode.float) [ southWest.lng, southWest.lat, northEast.lng, northEast.lat ]
