module BBox exposing (BBox, coordinates, fromCoordinates, fromExtrema)

import Angle exposing (Angle)
import Coordinates exposing (WGS84)


type BBox
    = BBox WGS84 WGS84


fromCoordinates : { southWest : WGS84, northEast : WGS84 } -> BBox
fromCoordinates inp =
    BBox inp.southWest inp.northEast


fromExtrema : { minLng : Float, minLat : Float, maxLng : Float, maxLat : Float } -> BBox
fromExtrema { minLng, minLat, maxLng, maxLat } =
    BBox { lng = Angle.degrees minLng, lat = Angle.degrees minLat }
        { lng = Angle.degrees maxLng, lat = Angle.degrees maxLat }


coordinates : BBox -> { southWest : WGS84, northEast : WGS84 }
coordinates (BBox southWest northEast) =
    { southWest = southWest, northEast = northEast }


everything : BBox
everything =
    fromExtrema { minLng = -180, minLat = -85, maxLng = 180, maxLat = 85 }
