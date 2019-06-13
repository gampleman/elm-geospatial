module Polygon exposing
    ( Polygon(..), LinearRing(..)
    , new, fromBBox
    , area, bbox
    , equal
    , containsPoint, intersection
    )

{-|

@docs Polygon, LinearRing


## Constructing

@docs new, fromBBox


## Measurement

@docs area, bbox


## Misc

@docs equal

-}

import Angle
import Area exposing (Area)
import BBox exposing (BBox)
import Coordinates exposing (WGS84)
import Dict
import Helpers exposing (pointMax, pointMin, pointToPoint2d)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Quantity exposing (toInt)
import Triangle2d
import Vector2d


{-| A polygon is a ring defining the outer perimeter with any number of holes within it.
-}
type Polygon coordinate
    = Polygon (LinearRing coordinate) (List (LinearRing coordinate))


{-| A LinearRing represents a loop of coordinates, i.e. a simple polygon.

The type constraints these to be at least triangle.

Note: Unlike GeoJSON we do not store the first coordinate redundantly at the end.

-}
type LinearRing coordinate
    = LinearRing coordinate coordinate coordinate (List coordinate)


{-| Creates a rectangular polygon from a BBox.
-}
fromBBox : BBox -> Polygon WGS84
fromBBox bBox =
    let
        { southWest, northEast } =
            BBox.coordinates bBox
    in
    Polygon (LinearRing southWest { lng = southWest.lng, lat = northEast.lat } northEast [ { lng = northEast.lng, lat = southWest.lat } ]) []


{-| This is a utility function for constructing simple polygons from literals.
-}
new : List { lng : Float, lat : Float } -> Maybe (Polygon WGS84)
new coords =
    let
        toWSG { lng, lat } =
            { lng = Angle.degrees lng, lat = Angle.degrees lat }
    in
    case coords of
        a :: b :: c :: rest ->
            Just (Polygon (LinearRing (toWSG a) (toWSG b) (toWSG c) (List.map toWSG rest)) [])

        _ ->
            Nothing


{-| While nothing prevents you from using normal elm equality (`==`), it may be too strict sometimes.

Polygons consist of rings, and so while order within the ring matters, where you start does not. Furthermore, the direction of the ring usually doesn't matter (although some software does care about this).

This equality function will make sure to align the rings properly before comparing them. Optionally you can instruct it to ingore the direction of the rings or only consider a number of significant decimal digits.

    1  a -- b     2  b -- c    3  a -- d
       |    |        |    |       |    |
       d -- c        a -- d       b -- c

    1: [(0, 0), (1, 0), (1, 1), (0, 1)]
    2: [(0, 1), (0, 0), (1, 0), (1, 1)]
    3: [(0, 0), (0, 1), (1, 1), (1, 0)]

    In this figure the three polygons are identical squares,
    but (1) and (2) differ in their starting point, whereas
    (1) and (3) differ in their direction. This function
    would consider (1) and (2) equal, but (1) and (3) only
    if you passed ignoreDirection = True.

-}
equal : { precision : Int, ignoreDirection : Bool } -> Polygon WGS84 -> Polygon WGS84 -> Bool
equal options (Polygon aOuter aHoles) (Polygon bOuter bHoles) =
    linearRingEqual options aOuter bOuter && List.all identity (List.map2 (linearRingEqual options) aHoles bHoles)


rotateToMatchStart : Int -> LinearRing WGS84 -> WGS84 -> Maybe (LinearRing WGS84)
rotateToMatchStart tolerance ((LinearRing _ _ _ rest) as a) coord =
    let
        helper steps (LinearRing a1 a2 a3 a4s) =
            if steps == 0 then
                Nothing

            else if Coordinates.equalWithPrecision tolerance a1 coord then
                Just (LinearRing a1 a2 a3 a4s)

            else
                case a4s of
                    [] ->
                        helper (steps - 1) (LinearRing a2 a3 a1 [])

                    h :: t ->
                        helper (steps - 1) (LinearRing a2 a3 h (t ++ [ a1 ]))
    in
    helper (List.length rest + 3) a


linearRingEqual : { precision : Int, ignoreDirection : Bool } -> LinearRing WGS84 -> LinearRing WGS84 -> Bool
linearRingEqual { precision, ignoreDirection } ((LinearRing a1 a2 a3 a4s) as a) ((LinearRing c1 c2 c3 c4s) as c) =
    if List.length a4s /= List.length c4s then
        False

    else
        case rotateToMatchStart precision c a1 of
            Just ((LinearRing b1 b2 b3 b4s) as b) ->
                if Coordinates.equalWithPrecision precision a2 b2 then
                    Coordinates.equalWithPrecision precision a3 b3
                        && List.all identity (List.map2 (Coordinates.equalWithPrecision precision) a4s b4s)

                else if ignoreDirection then
                    -- we'll attept to reverse the second ring
                    List.all identity
                        (List.map2 (Coordinates.equalWithPrecision precision)
                            (a1 :: a2 :: a3 :: a4s)
                            (b1 :: List.reverse b4s ++ [ b3, b2 ])
                        )

                else
                    False

            Nothing ->
                False


{-| Computes the area of the polygon.
-}
area : Polygon WGS84 -> Area
area (Polygon outside holes) =
    Quantity.abs (ringArea outside) |> Quantity.minus (Quantity.abs <| Quantity.sum <| List.map ringArea holes)


ringArea : LinearRing WGS84 -> Area
ringArea (LinearRing a b c rest) =
    let
        radius =
            6378137

        helper total list =
            case list of
                [ y, z ] ->
                    total + (Angle.inRadians a.lng - Angle.inRadians y.lng) * Angle.sin z.lat + (Angle.inRadians b.lng - Angle.inRadians z.lng) * Angle.sin a.lat

                p1 :: p2 :: p3 :: tail ->
                    helper (total + (Angle.inRadians p3.lng - Angle.inRadians p1.lng) * Angle.sin p2.lat) (p2 :: p3 :: tail)

                _ ->
                    -- crash
                    helper total list
    in
    helper 0 (a :: b :: c :: rest ++ [ a ]) * radius * radius / 2 |> Area.squareMeters


bbox : Polygon WGS84 -> BBox
bbox (Polygon (LinearRing a b c ds) _) =
    BBox.fromCoordinates <|
        List.foldr (\e f -> { southWest = pointMin e f.southWest, northEast = pointMax e f.northEast })
            { southWest = pointMin (pointMin a b) c, northEast = pointMax (pointMax a b) c }
            ds


containsPoint : WGS84 -> Polygon WGS84 -> Bool
containsPoint point poly =
    elmGeometryContains (pointToPoint2d point) (polygonToElmGeom poly)


polygonToElmGeom : Polygon WGS84 -> Polygon2d
polygonToElmGeom (Polygon outerRing innerRings) =
    Polygon2d.with { outerLoop = linearRingToElmGeom outerRing, innerLoops = List.map linearRingToElmGeom innerRings }


linearRingToElmGeom : LinearRing WGS84 -> List Point2d
linearRingToElmGeom (LinearRing a b c ds) =
    pointToPoint2d a :: pointToPoint2d b :: pointToPoint2d c :: List.map pointToPoint2d ds


polygon2dToPolygon : Polygon2d -> Maybe (Polygon WGS84)
polygon2dToPolygon poly =
    Polygon2d.outerLoop poly
        |> List.map (\point -> { lng = point |> Point2d.xCoordinate, lat = point |> Point2d.yCoordinate })
        |> List.reverse
        |> new


intersection : Polygon WGS84 -> Polygon WGS84 -> Maybe (Polygon WGS84)
intersection polyA polyB =
    elmGeometryIntersect (polygonToElmGeom polyA) (polygonToElmGeom polyB)
        |> polygon2dToPolygon



-- This code has been merged into elm-geometry, but hasn't been released yet, so it is temporarily here


elmGeometryContains : Point2d -> Polygon2d -> Bool
elmGeometryContains point polygon =
    containsPointHelp (Polygon2d.edges polygon) (Point2d.xCoordinate point) (Point2d.yCoordinate point) 0


containsPointHelp : List LineSegment2d -> Float -> Float -> Int -> Bool
containsPointHelp edgeList xp yp k =
    -- Based on Hao, J.; Sun, J.; Chen, Y.; Cai, Q.; Tan, L. Optimal Reliable Point-in-Polygon Test and
    -- Differential Coding Boolean Operations on Polygons. Symmetry 2018, 10, 477.
    -- https://www.mdpi.com/2073-8994/10/10/477/pdf
    case edgeList of
        [] ->
            not (modBy 2 k == 0)

        edge :: rest ->
            let
                ( p0, p1 ) =
                    LineSegment2d.endpoints edge

                ( xi, yi ) =
                    Point2d.coordinates p0

                ( xi1, yi1 ) =
                    Point2d.coordinates p1

                v1 =
                    yi - yp

                v2 =
                    yi1 - yp
            in
            if (v1 < 0 && v2 < 0) || (v1 > 0 && v2 > 0) then
                -- case 11 or 26
                containsPointHelp rest xp yp k

            else
                let
                    u1 =
                        xi - xp

                    u2 =
                        xi1 - xp
                in
                if v2 > 0 && v1 <= 0 then
                    let
                        f =
                            u1 * v2 - u2 * v1
                    in
                    if f > 0 then
                        -- case 3 or 9
                        containsPointHelp rest xp yp (k + 1)

                    else if f == 0 then
                        -- case 16 or 21
                        True

                    else
                        -- case 13 or 24
                        containsPointHelp rest xp yp k

                else if v1 > 0 && v2 <= 0 then
                    let
                        f =
                            u1 * v2 - u2 * v1
                    in
                    if f < 0 then
                        -- case 4 or 10
                        containsPointHelp rest xp yp (k + 1)

                    else if f == 0 then
                        -- case 19 or 20
                        True

                    else
                        -- case 12 or 25
                        containsPointHelp rest xp yp k

                else if v2 == 0 && v1 < 0 then
                    let
                        f =
                            u1 * v2 - u2 * v1
                    in
                    if f == 0 then
                        -- case 17
                        True

                    else
                        -- case 7 or 14
                        containsPointHelp rest xp yp k

                else if v1 == 0 && v2 < 0 then
                    let
                        f =
                            u1 * v2 - u2 * v1
                    in
                    if f == 0 then
                        -- case 18
                        True

                    else
                        -- case 8 or 15
                        containsPointHelp rest xp yp k

                else if v1 == 0 && v2 == 0 then
                    if u2 <= 0 && u1 >= 0 then
                        -- case 1
                        True

                    else if u1 <= 0 && u2 >= 0 then
                        -- case 2
                        True

                    else
                        --  case 5, 6, 22, 23
                        containsPointHelp rest xp yp k

                else
                    containsPointHelp rest xp yp k



-- This code is horible, but should eventually be moved to elm-geometry as Well


lineIntersection : LineSegment2d -> LineSegment2d -> List Point2d
lineIntersection lineSegment1 lineSegment2 =
    -- this is basically the same as LineSegment2d.intersectionPoint, but we want to treat co-linear differently
    -- The two line segments are:
    -- p |--- r ---| p_
    -- q |--- s ---| q_
    let
        ( p, p_ ) =
            LineSegment2d.endpoints lineSegment1

        ( q, q_ ) =
            LineSegment2d.endpoints lineSegment2

        r =
            LineSegment2d.vector lineSegment1

        s =
            LineSegment2d.vector lineSegment2

        pq =
            Vector2d.from p q

        pq_ =
            Vector2d.from p q_

        qp_ =
            Vector2d.from q p_

        pqXr =
            Vector2d.crossProduct pq r

        pqXs =
            Vector2d.crossProduct pq s

        sXqp_ =
            Vector2d.crossProduct s qp_

        rXpq_ =
            Vector2d.crossProduct r pq_

        tDenominator =
            pqXs - sXqp_

        uDenominator =
            pqXr + rXpq_
    in
    if tDenominator == 0 || uDenominator == 0 then
        if Vector2d.dotProduct r s < 0 then
            if p_ == q_ then
                -- p |----- p_ | q_ -----| q
                []

            else if p == q then
                -- q_ |----- q | p -----| p_
                []

            else if Triangle2d.area (Triangle2d.fromVertices ( p, p_, q )) < 0.00001 then
                [ p, p_ ]
                    |> List.filter (\point -> abs (Point2d.distanceFrom point q + Point2d.distanceFrom point q_ - Point2d.distanceFrom q q_) < 0.00001)

            else
                []

        else if p_ == q then
            -- p |----- p_ | q -----| q_
            []

        else if p == q_ then
            -- q |----- q_ | p -----| p_
            []

        else if Triangle2d.area (Triangle2d.fromVertices ( p, p_, q )) < 0.00001 then
            [ p, p_ ]
                |> List.filter (\point -> abs (Point2d.distanceFrom point q + Point2d.distanceFrom point q_ - Point2d.distanceFrom q q_) < 0.00001)

        else
            []

    else
        -- Segments are not parallel.
        -- We search for the intersection point of the two lines.
        let
            t =
                pqXs / tDenominator

            u =
                pqXr / uDenominator
        in
        if (0 <= t && t <= 1) && (0 <= u && u <= 1) then
            -- Intersection is within both segments.
            let
                -- Ensure interpolation happens from the closest
                -- endpoint (this should be more numerically stable, and
                -- also mostly ensures that intersection is symmetric)
                intersection_ =
                    if min t (1 - t) <= min u (1 - u) then
                        LineSegment2d.interpolate lineSegment1 t

                    else
                        LineSegment2d.interpolate lineSegment2 u
            in
            [ intersection_ ]

        else
            []


listShift : List a -> List a
listShift aList =
    case aList of
        [] ->
            []

        x :: xs ->
            xs ++ [ x ]


removeAdjacentDuplicates : List Point2d -> List Point2d -> List Point2d
removeAdjacentDuplicates accu list =
    case list of
        [] ->
            List.reverse accu

        [ x ] ->
            case List.reverse accu of
                [] ->
                    [ x ]

                head :: rest ->
                    if x == head then
                        head :: rest

                    else
                        head :: rest ++ [ x ]

        x :: y :: rest ->
            if x == y then
                removeAdjacentDuplicates accu (y :: rest)

            else
                removeAdjacentDuplicates (x :: accu) (y :: rest)


buildPoints : List LineSegment2d -> Point2d -> Point2d -> List Point2d
buildPoints otherEdges start end =
    let
        segment =
            LineSegment2d.from start end
    in
    start
        :: List.concatMap
            (lineIntersection segment)
            otherEdges
        |> List.sortBy (Point2d.distanceFrom start)



-- let's start with a super inneficient algorithm - this is O(n*m)
-- but this can be later optimized with a circular plane sweep algorithm


subdivide : ( Polygon2d, Polygon2d ) -> ( Polygon2d, Polygon2d )
subdivide ( polyA, polyB ) =
    let
        edgesA =
            Polygon2d.edges polyA

        edgesB =
            Polygon2d.edges polyB
    in
    ( Polygon2d.with { outerLoop = removeAdjacentDuplicates [] <| List.concat <| List.map2 (buildPoints edgesB) (Polygon2d.outerLoop polyA) (listShift (Polygon2d.outerLoop polyA)), innerLoops = [] }
    , Polygon2d.with { outerLoop = removeAdjacentDuplicates [] <| List.concat <| List.map2 (buildPoints edgesA) (Polygon2d.outerLoop polyB) (listShift (Polygon2d.outerLoop polyB)), innerLoops = [] }
    )


elmGeometryIntersect : Polygon2d -> Polygon2d -> Polygon2d
elmGeometryIntersect polyA polyB =
    let
        ( polyAS, polyBS ) =
            subdivide ( polyA, polyB )

        toKey p =
            Tuple.mapBoth (\a -> round (a * 100000000)) (\a -> round (a * 100000000)) <| Point2d.coordinates p

        addIfInside poly edge dict =
            let
                ( start, end ) =
                    LineSegment2d.endpoints edge

                midpoint =
                    LineSegment2d.midpoint edge
            in
            if elmGeometryContains midpoint poly then
                if toKey start == toKey end then
                    dict

                else
                    Dict.insert (toKey start) end dict

            else
                dict

        edgeDictA =
            Polygon2d.edges polyAS
                |> List.foldr (addIfInside polyB) Dict.empty

        edgeDict =
            Polygon2d.edges polyBS
                |> List.foldr (addIfInside polyA) edgeDictA

        randomPoint =
            edgeDict |> Dict.toList |> List.head

        buildChain beginCoords end soFar =
            case Dict.get (toKey end) edgeDict of
                Just point ->
                    if toKey point == beginCoords then
                        Polygon2d.singleLoop (List.reverse (point :: soFar))

                    else
                        buildChain beginCoords point (point :: soFar)

                Nothing ->
                    Polygon2d.singleLoop soFar
    in
    case randomPoint of
        Nothing ->
            Polygon2d.singleLoop []

        Just ( startCoords, end ) ->
            buildChain startCoords end [ end ]



-- polyAS
