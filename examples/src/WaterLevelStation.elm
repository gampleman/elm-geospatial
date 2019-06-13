module WaterLevelStation exposing (Status(..), WaterLevelStation(..), decoder, getStatus, source)

import Coordinates exposing (WGS84)
import FeatureCollection exposing (FeatureCollection)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import Mapbox.Source exposing (Source)


type WaterLevelStation
    = Off
    | On Status


type Status
    = Normal
    | Alert
    | Danger


getStatus : WaterLevelStation -> Maybe Status
getStatus wlStation =
    case wlStation of
        Off ->
            Nothing

        On status ->
            Just status


source : String -> FeatureCollection WGS84 Status -> Source
source id collection =
    collection
        |> FeatureCollection.encode encodeStatus
        |> Mapbox.Source.geoJSONFromValue id []


encodeStatus : Status -> Json.Encode.Value
encodeStatus v =
    Json.Encode.object
        [ ( "status"
          , case v of
                Normal ->
                    Json.Encode.string "normal"

                Alert ->
                    Json.Encode.string "alert"

                Danger ->
                    Json.Encode.string "danger"
          )
        ]


decoder : Decoder WaterLevelStation
decoder =
    Decode.field "wlstationstatus" Decode.string
        |> Decode.andThen
            (\stationStatus ->
                case stationStatus of
                    "ON" ->
                        Decode.field "waterlevelmsg" Decode.string
                            |> Decode.andThen
                                (\msg ->
                                    case msg of
                                        "NORMAL" ->
                                            Decode.succeed Normal

                                        "ALERT" ->
                                            Decode.succeed Alert

                                        "DANGER" ->
                                            Decode.succeed Danger

                                        _ ->
                                            Decode.fail <| "unknown wl message " ++ msg
                                )
                            |> Decode.map On

                    "OFF" ->
                        Decode.succeed Off

                    _ ->
                        Decode.fail <| "Unkown wl status " ++ stationStatus
            )
