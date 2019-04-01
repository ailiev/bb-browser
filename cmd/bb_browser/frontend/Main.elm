module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation as Navigation
import Html exposing (h1, span, text)
import Html.Attributes exposing (class, href)
import Platform.Cmd
import Platform.Sub
import Route
import Url exposing (Url)



-- MODEL


type Model
    = NotFound
    | Welcome


type alias Flags =
    {}


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
    changeRouteTo (Route.fromUrl url) NotFound


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( NotFound, Cmd.none )

        Just (Route.Action _) ->
            ( NotFound, Cmd.none )

        Just (Route.Command _) ->
            ( NotFound, Cmd.none )

        Just (Route.Directory _) ->
            ( NotFound, Cmd.none )

        Just (Route.Tree _) ->
            ( NotFound, Cmd.none )

        Just (Route.UncachedActionResult _) ->
            ( NotFound, Cmd.none )

        Just Route.Welcome ->
            ( Welcome, Cmd.none )



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


viewPage : List (Html.Html Msg) -> Browser.Document Msg
viewPage contents =
    Browser.Document "Buildbarn Browser"
        [ Html.nav
            [ class "navbar", class "navbar-dark", class "bg-primary" ]
            [ span [ class "navbar-brand" ] [ text "Buildbarn Browser" ] ]
        , Grid.container [] contents
        ]


view : Model -> Browser.Document Msg
view model =
    viewPage [ h1 [] [ text "Heading 1" ] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
