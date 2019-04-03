module Page.NotFound exposing (view)

import Html exposing (p, text)
import Page


view : Page.Page msg
view =
    { title = "Page not found"
    , bannerColor = "danger"
    , body = [ p [] [ text "The URL that was requested does not correspond to a page." ] ]
    }
