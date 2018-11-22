module Coordinates exposing (WGS84)

import Angle exposing (Angle)


type alias WGS84 =
    { lat : Angle
    , lng : Angle
    }
