module Page exposing (Page)

import Html


type alias Page msg =
    { title : String
    , bannerColor : String
    , body : List (Html.Html msg)
    }
