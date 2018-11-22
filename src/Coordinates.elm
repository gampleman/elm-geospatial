module Coordinates exposing (WGS84, wgs84Decoder)

import Angle exposing (Angle)
import Json.Decode as Decode exposing (Decoder)


type alias WGS84 =
    { lat : Angle
    , lng : Angle
    }


wgs84Decoder : Decoder WGS84
wgs84Decoder =
    Decode.list Decode.float
        |> Decode.andThen
            (\l ->
                case l of
                    [ lng, lat ] ->
                        Decode.succeed { lng = Angle.degrees lng, lat = Angle.degrees lat }

                    _ ->
                        Decode.fail "Wrong number of coordinates"
            )
