module RainfallStation exposing (RainfallStation, Status, decoder, getStatus, source)

import Coordinates exposing (WGS84)
import FeatureCollection exposing (FeatureCollection)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import Mapbox.Source exposing (Source)


type RainfallStation
    = Off
    | On Status


type Status
    = NoRain
    | Light
    | Moderate
    | Heavy
    | VeryHeavy


getStatus : RainfallStation -> Maybe Status
getStatus rainfallStation =
    case rainfallStation of
        Off ->
            Nothing

        On status ->
            Just status


decoder : Decoder RainfallStation
decoder =
    Decode.field "rfstationstatus" Decode.string
        |> Decode.andThen
            (\stationStatus ->
                case stationStatus of
                    "ON" ->
                        Decode.field "rainfallmsg" Decode.string
                            |> Decode.andThen
                                (\msg ->
                                    case msg of
                                        "NODATA" ->
                                            Decode.succeed NoRain

                                        "LIGHT" ->
                                            Decode.succeed Light

                                        "MODERATE" ->
                                            Decode.succeed Moderate

                                        "HEAVY" ->
                                            Decode.succeed Heavy

                                        "VERY HEAVY" ->
                                            Decode.succeed VeryHeavy

                                        _ ->
                                            Decode.fail <| "unknown rf message " ++ msg
                                )
                            |> Decode.map On

                    "OFF" ->
                        Decode.succeed Off

                    _ ->
                        Decode.fail <| "Unkown rf status " ++ stationStatus
            )


source : String -> FeatureCollection WGS84 Status -> Source
source id collection =
    collection
        |> FeatureCollection.encode encodeStatus
        |> Mapbox.Source.geoJSONFromValue id [ Mapbox.Source.tolerance 0 ]


encodeStatus : Status -> Json.Encode.Value
encodeStatus v =
    Json.Encode.object
        [ ( "status"
          , case v of
                NoRain ->
                    Json.Encode.string "no-rain"

                Light ->
                    Json.Encode.string "light"

                Moderate ->
                    Json.Encode.string "moderate"

                Heavy ->
                    Json.Encode.string "heavy"

                VeryHeavy ->
                    Json.Encode.string "very-heavy"
          )
        ]
