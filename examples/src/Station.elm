module Station exposing (Station(..), decoder, getRainfall, getWaterLevel)

import Json.Decode as Decode exposing (Decoder)
import RainfallStation exposing (RainfallStation)
import WaterLevelStation exposing (WaterLevelStation)


type Station
    = WaterLevel WaterLevelStation
    | Rainfall RainfallStation
    | Combined WaterLevelStation RainfallStation
    | Garbage


decoder : Decoder Station
decoder =
    Decode.field "stationtype" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "WL" ->
                        Decode.map WaterLevel WaterLevelStation.decoder
                            |> failover

                    "RF" ->
                        Decode.map Rainfall RainfallStation.decoder
                            |> failover

                    "RF & WL" ->
                        Decode.oneOf
                            [ Decode.map2 Combined WaterLevelStation.decoder RainfallStation.decoder
                            , Decode.map WaterLevel WaterLevelStation.decoder
                            , Decode.map Rainfall RainfallStation.decoder
                            , Decode.succeed Garbage
                            ]

                    _ ->
                        Decode.fail "Unexpected type"
            )


failover : Decoder Station -> Decoder Station
failover stationDecoder =
    Decode.oneOf
        [ stationDecoder
        , Decode.succeed Garbage
        ]


getRainfall : Station -> Maybe RainfallStation
getRainfall station =
    case station of
        WaterLevel wl ->
            Nothing

        Rainfall rf ->
            Just rf

        Combined wl rf ->
            Just rf

        Garbage ->
            Nothing


getWaterLevel : Station -> Maybe WaterLevelStation
getWaterLevel station =
    case station of
        WaterLevel wl ->
            Just wl

        Rainfall rf ->
            Nothing

        Combined wl rf ->
            Just wl

        Garbage ->
            Nothing
