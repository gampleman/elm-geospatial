module TestHelpers exposing (equalWithinTolerance, listsEquivalent, multiplePolygonsEqual, polygonsEqual)

import Angle
import Coordinates exposing (WGS84)
import Expect exposing (Expectation, FloatingPointTolerance)
import Feature exposing (Feature(..))
import Float.Extra as Float
import GeoCollection exposing (encode)
import Json.Encode
import Polygon exposing (LinearRing(..), Polygon(..))
import Regex
import Result
import String
import Test.Runner exposing (getFailureReason)
import Test.Runner.Failure
import Url.Builder


listsEquivalent : (a -> b -> Expectation) -> List a -> List b -> Expectation
listsEquivalent function aList bList =
    if List.length aList == List.length bList then
        let
            failures =
                List.map2 function aList bList
                    |> List.filterMap getFailureReason
        in
        if List.length failures > 0 then
            Expect.fail ("Lists were not equal: \n" ++ format (List.head failures))

        else
            Expect.pass

    else
        Expect.fail "Lists have different lengths"


format =
    Maybe.withDefault "" << Maybe.map (\r -> Test.Runner.Failure.format r.description r.reason)


multiplePolygonsEqual : { precision : Int, ignoreDirection : Bool } -> List (Polygon WGS84) -> List (Polygon WGS84) -> Expectation
multiplePolygonsEqual opts xs ys =
    if List.length xs /= List.length ys then
        Expect.equalLists xs ys

    else
        List.map2 (Polygon.equal opts) xs ys
            |> List.all identity
            |> Expect.true ("The polygons do not match: \n  compare: " ++ dataUrl xs ys)


polygonsEqual : { precision : Int, ignoreDirection : Bool } -> Polygon WGS84 -> Polygon WGS84 -> Expectation
polygonsEqual opts a b =
    Expect.true ("The polygons do not match: \n  compare: " ++ dataUrl [ a ] [ b ]) <| Polygon.equal opts a b


dataUrl : List (Polygon WGS84) -> List (Polygon WGS84) -> String
dataUrl expected actual =
    encode
        (\exp ->
            Json.Encode.object
                (if exp then
                    [ ( "fill", Json.Encode.string "green" ), ( "label", Json.Encode.string "expected" ) ]

                 else
                    [ ( "fill", Json.Encode.string "red" ), ( "label", Json.Encode.string "actual" ) ]
                )
        )
        (always [])
        (List.map (\exp -> Polygons [ exp ] True) expected ++ List.map (\act -> Polygons [ act ] False) actual)
        |> Json.Encode.encode 0
        |> String.append "data:application/json,"
        |> Url.Builder.string "data"
        |> List.singleton
        |> Url.Builder.toQuery
        |> String.replace "?" "#"
        |> String.append "http://geojson.io/"


equalWithinTolerance : a -> a -> Expectation
equalWithinTolerance ia ib =
    let
        strA =
            Debug.toString ia

        strB =
            Debug.toString ib

        anyNumberRegExpr =
            Regex.fromString "(-?[0-9]+(?:\\.[0-9]+)?)"
                |> Maybe.withDefault Regex.never

        removeAllNumbers str =
            Regex.replace anyNumberRegExpr (always "") str

        extractAllNumbers str =
            Regex.find anyNumberRegExpr str
                |> List.map .match

        sameNumber a b =
            let
                numA =
                    String.toFloat a

                numB =
                    String.toFloat b
            in
            Maybe.map2 (Float.equalWithin 1.0e-4) numA numB
                |> Maybe.withDefault False

        sameNumbers =
            let
                numbersA =
                    extractAllNumbers strA

                numbersB =
                    extractAllNumbers strB

                sameLength =
                    List.length numbersA == List.length numbersB
            in
            sameLength
                && (List.map2 sameNumber numbersA numbersB
                        |> List.all identity
                   )
    in
    if removeAllNumbers strA == removeAllNumbers strB && sameNumbers then
        Expect.pass

    else
        -- this custom test fails, assume that a and b are really different (even by this test's rules)
        -- use Expect.equal to get a nicely formatted output
        Expect.equal ia ib
