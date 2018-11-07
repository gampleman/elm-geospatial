module LineString exposing (LineString(..))

import GeoPosition exposing (GeoPosition)


type LineString
    = LineString GeoPosition GeoPosition (List GeoPosition)
