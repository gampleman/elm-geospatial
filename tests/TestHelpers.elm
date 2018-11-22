module TestHelpers exposing (equalWithinTolerance)

import Expect exposing (Expectation)
import Float.Extra as Float
import Regex
import Result
import String


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
