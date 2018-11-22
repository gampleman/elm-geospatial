module LineString exposing (LineString(..))


type LineString coordinate
    = LineString coordinate coordinate (List coordinate)
