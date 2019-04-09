module Buildbarn.Browser.Frontend.Page exposing (Page, viewError, viewLoading, viewPage)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing exposing (mb5, my4)
import Browser
import Html exposing (a, h1, p, text)
import Html.Attributes exposing (class, href)
import Http


type alias Page msg =
    { title : String
    , bannerColor : String
    , body : List (Html.Html msg)
    }


viewPage : Page msg -> Browser.Document msg
viewPage contents =
    Browser.Document ("Buildbarn Browser - " ++ contents.title)
        [ Html.nav
            [ class "navbar"
            , class "navbar-dark"
            , class ("bg-" ++ contents.bannerColor)
            ]
            [ a [ class "navbar-brand", href "#" ] [ text "Buildbarn Browser" ] ]
        , Grid.container [ mb5 ] ([ h1 [ my4 ] [ text contents.title ] ] ++ contents.body)
        ]



-- Displays a HTTP error message in a friendly way.


viewError : Http.Error -> List (Html.Html msg)
viewError error =
    [ p []
        [ text
            (case error of
                Http.BadUrl message ->
                    "BadURL " ++ message

                Http.Timeout ->
                    "Timeout"

                Http.NetworkError ->
                    "Network error"

                Http.BadStatus code ->
                    "BadCode " ++ String.fromInt code

                Http.BadBody message ->
                    "BadBody " ++ message
            )
        ]
    ]



-- Displays a message that the page is still loading.


viewLoading : List (Html.Html msg)
viewLoading =
    [ p [] [ text "Loading..." ] ]
