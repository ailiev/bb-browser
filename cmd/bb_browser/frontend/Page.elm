module Page exposing (Page)

import Html


type alias Page msg =
    { title : String
    , banner_color : String
    , body : List (Html.Html msg)
    }
