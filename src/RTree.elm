module RTree exposing (RTree, containsPoint, empty, insert)

import Angle exposing (Angle)
import Area exposing (Area)
import BBox exposing (BBox)
import Coordinates exposing (WGS84)
import Feature exposing (Feature)
import Point
import Quantity


type RTree a
    = RTree (RTreeInnards a)


type alias RTreeInnards a =
    { maxEntries : Int
    , minEntries : Int
    , data : NodeAlias a
    }


type alias NodeAlias a =
    { children : NodeContents a
    , bbox : BBox
    }


type NodeContents a
    = Node (List (NodeAlias a))
    | Leaf (List (Feature WGS84 a))


empty : RTree a
empty =
    withNodeSize 9


containsPoint : WGS84 -> RTree a -> Bool
containsPoint point (RTree tree) =
    containsPointHelp point tree.data


containsPointHelp : WGS84 -> NodeAlias a -> Bool
containsPointHelp point node =
    if BBox.containsPoint point node.bbox then
        case node.children of
            Node children ->
                List.any (containsPointHelp point) children

            Leaf children ->
                List.any (Feature.containsPoint point) children

    else
        False


infinity =
    1 / 0


nullBBox : BBox
nullBBox =
    BBox.fromExtrema { minLng = infinity, minLat = infinity, maxLng = -infinity, maxLat = -infinity }


withNodeSize : Int -> RTree a
withNodeSize maxEntries =
    RTree
        { maxEntries = max 4 maxEntries
        , minEntries = max 2 (ceiling (toFloat maxEntries * 0.4))
        , data =
            { children = Leaf []
            , bbox = nullBBox
            }
        }


insert : Feature WGS84 a -> RTree a -> RTree a
insert feature (RTree tree) =
    let
        bbox =
            Feature.bbox feature

        newNodes =
            insertHelp tree feature bbox tree.data
    in
    case newNodes of
        [ single ] ->
            RTree { tree | data = single }

        multiple ->
            RTree { tree | data = { children = Node multiple, bbox = unifyBBox (List.map .bbox multiple) } }


insertHelp : RTreeInnards a -> Feature WGS84 a -> BBox -> NodeAlias a -> List (NodeAlias a)
insertHelp tree item bbox node =
    case node.children of
        Node ((first :: rest) as children) ->
            let
                firstArea =
                    BBox.area first.bbox

                targetChild =
                    List.foldl
                        (\child ({ targetNode, minArea, minEnlargement } as sum) ->
                            let
                                area =
                                    BBox.area child.bbox

                                enlargment =
                                    enlargedArea child.bbox bbox |> Quantity.minus area
                            in
                            if enlargment |> Quantity.lessThan minEnlargement then
                                { targetNode = child, minArea = Quantity.min area minArea, minEnlargement = enlargment }

                            else if enlargment == minEnlargement && (area |> Quantity.lessThan minArea) then
                                { targetNode = child, minArea = area, minEnlargement = enlargment }

                            else
                                sum
                        )
                        { targetNode = first, minArea = firstArea, minEnlargement = enlargedArea first.bbox bbox |> Quantity.minus firstArea }
                        rest

                newNodes =
                    insertHelp tree item bbox targetChild.targetNode
            in
            { node
                | children =
                    Node <|
                        List.concatMap
                            (\child ->
                                if child == targetChild.targetNode then
                                    newNodes

                                else
                                    [ child ]
                            )
                            children
                , bbox = BBox.union node.bbox bbox
            }
                |> splitIfRequired tree

        Node [] ->
            [ { node | children = Leaf [ item ], bbox = BBox.union node.bbox bbox } ]

        Leaf children ->
            { node | children = Leaf (item :: children), bbox = BBox.union node.bbox bbox }
                |> splitIfRequired tree


enlargedArea : BBox -> BBox -> Area
enlargedArea a b =
    BBox.union a b |> BBox.area


bboxMargin : BBox -> Angle
bboxMargin bbox =
    let
        { southWest, northEast } =
            BBox.coordinates bbox
    in
    (northEast.lat |> Quantity.minus southWest.lat) |> Quantity.plus (northEast.lng |> Quantity.minus southWest.lng)


unifyBBox : List BBox -> BBox
unifyBBox =
    List.foldl BBox.union nullBBox


allDistMargin : Int -> Int -> List BBox -> Angle
allDistMargin min total bboxes =
    let
        leftBBox =
            bboxes
                |> List.take min
                |> unifyBBox

        rightBBox =
            bboxes
                |> List.reverse
                |> List.take min
                |> List.foldl BBox.union nullBBox

        margin =
            bboxMargin leftBBox |> Quantity.plus (bboxMargin rightBBox)

        rest =
            bboxes |> List.drop min |> List.reverse |> List.drop min

        ( _, margin1 ) =
            List.foldr
                (\bbox ( totalBBox, totalMargin ) ->
                    let
                        newBBox =
                            BBox.union totalBBox bbox
                    in
                    ( newBBox, totalMargin |> Quantity.plus (bboxMargin newBBox) )
                )
                ( leftBBox, margin )
                rest

        ( _, margin2 ) =
            List.foldl
                (\bbox ( totalBBox, totalMargin ) ->
                    let
                        newBBox =
                            BBox.union totalBBox bbox
                    in
                    ( newBBox, totalMargin |> Quantity.plus (bboxMargin newBBox) )
                )
                ( rightBBox, margin1 )
                rest
    in
    margin2


type ItemContainer a
    = NodeItem (NodeAlias a)
    | FeatureItem (Feature WGS84 a)


splitIfRequired : RTreeInnards a -> NodeAlias a -> List (NodeAlias a)
splitIfRequired tree node =
    let
        childCount =
            case node.children of
                Node c ->
                    List.length c

                Leaf c ->
                    List.length c
    in
    if childCount > tree.maxEntries then
        split tree node

    else
        [ node ]


split : RTreeInnards a -> NodeAlias a -> List (NodeAlias a)
split tree node =
    let
        childCount =
            List.length withBBoxes

        withBBoxes =
            case node.children of
                Node c ->
                    List.map (\a -> ( NodeItem a, a.bbox )) c

                Leaf c ->
                    List.map (\a -> ( FeatureItem a, Feature.bbox a )) c

        sortedByX =
            List.sortBy (Tuple.second >> BBox.minLng >> Angle.inRadians) withBBoxes

        sortedByY =
            List.sortBy (Tuple.second >> BBox.minLat >> Angle.inRadians) withBBoxes

        computeAllDistMargin =
            List.map Tuple.second >> allDistMargin tree.minEntries childCount

        sorted =
            if computeAllDistMargin sortedByX |> Quantity.lessThan (computeAllDistMargin sortedByY) then
                sortedByX

            else
                sortedByY

        ( leftList, rightList ) =
            splitHelper (List.drop tree.minEntries sorted) (List.take tree.minEntries sorted) (childCount - tree.minEntries) (Area.squareMeters infinity) (Area.squareMeters infinity) ( [], [] )

        reconstructChildren list =
            case
                List.foldr
                    (\( a, _ ) ( b, c ) ->
                        case a of
                            NodeItem d ->
                                ( d :: b, c )

                            FeatureItem d ->
                                ( b, d :: c )
                    )
                    ( [], [] )
                    list
            of
                ( a, [] ) ->
                    Node a

                ( _, a ) ->
                    Leaf a

        computeBBox =
            List.foldl (\a b -> BBox.union (Tuple.second a) b) nullBBox
    in
    [ { node | children = reconstructChildren leftList, bbox = computeBBox leftList }
    , { node | children = reconstructChildren rightList, bbox = computeBBox rightList }
    ]



-- ATM this will slightly shuffle order in list2 :(


splitHelper list1 list2 steps minOverlap minArea bestSoFar =
    case ( steps, list1 ) of
        ( 0, _ ) ->
            bestSoFar

        ( _, [] ) ->
            bestSoFar |> Debug.log "shit happened"

        -- should never happen
        ( _, x :: xs ) ->
            let
                bbox1 =
                    List.foldl (\a b -> BBox.union (Tuple.second a) b) nullBBox list1

                bbox2 =
                    List.foldl (\a b -> BBox.union (Tuple.second a) b) nullBBox list2

                overlap =
                    BBox.intersection bbox1 bbox2 |> BBox.area

                area =
                    BBox.area bbox1 |> Quantity.plus (BBox.area bbox2)

                ( best, minOverlap1, minArea1 ) =
                    if overlap |> Quantity.lessThan minOverlap then
                        ( ( list1, list2 ), overlap, Quantity.min minArea area )

                    else if overlap == minOverlap && (area |> Quantity.lessThan minArea) then
                        ( ( list1, list2 ), overlap, area )

                    else
                        ( bestSoFar, minOverlap, minArea )
            in
            splitHelper xs (x :: list2) (steps - 1) minOverlap1 minArea1 best
