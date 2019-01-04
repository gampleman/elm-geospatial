module BBoxTest exposing (bboxDecodingTest, bboxEncodingTest)

import BBox exposing (BBox)
import Expect
import Json.Decode
import Json.Encode
import Test exposing (Test, describe, test)


bboxDecodingTest : Test
bboxDecodingTest =
    describe "decoding"
        [ test "decodes from GeoJSON representation correctly" <|
            \() ->
                Json.Decode.decodeString BBox.decoder "[30, 40, 90, 120]"
                    |> Expect.equal (Ok (BBox.fromExtrema { minLng = 30, minLat = 40, maxLng = 90, maxLat = 120 }))
        ]


bboxEncodingTest : Test
bboxEncodingTest =
    describe "encoding"
        [ test "encodes to GeoJSON correcly" <|
            \() ->
                BBox.fromExtrema { minLng = 30, minLat = 40, maxLng = 90, maxLat = 120 }
                    |> BBox.encode
                    |> Json.Encode.encode 0
                    |> Expect.equal "[30,40,90,120]"
        ]
