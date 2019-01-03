module Coordinates exposing (WGS84, equalWithPrecision, wgs84Decoder)

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


equalWithPrecision : Int -> WGS84 -> WGS84 -> Bool
equalWithPrecision precision a b =
    let
        tolerance =
            1 / 10 ^ toFloat precision

        alat =
            Angle.inDegrees a.lat

        alng =
            Angle.inDegrees a.lng

        blat =
            Angle.inDegrees b.lat

        blng =
            Angle.inDegrees b.lng
    in
    alat - tolerance <= blat && blat <= alat + tolerance && alng - tolerance <= blng && blng <= alng + tolerance
