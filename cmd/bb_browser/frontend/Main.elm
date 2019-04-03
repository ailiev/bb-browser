module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation as Navigation
import Html exposing (a, h1, text)
import Html.Attributes exposing (class, href)
import Page
import Page.NotFound
import Page.Welcome
import Platform.Cmd
import Platform.Sub
import Route
import Url exposing (Url)



-- MODEL


type CurrentPage
    = NotFound
    | Welcome


type alias Model =
    { currentPage : CurrentPage
    , navigationKey : Navigation.Key
    }


type alias Flags =
    {}


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
    changeRouteTo (Route.fromUrl url)
        { currentPage = NotFound
        , navigationKey = navigationKey
        }


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | currentPage = NotFound }, Cmd.none )

        Just (Route.Action _) ->
            ( { model | currentPage = NotFound }, Cmd.none )

        Just (Route.Command _) ->
            ( { model | currentPage = NotFound }, Cmd.none )

        Just (Route.Directory _) ->
            ( { model | currentPage = NotFound }, Cmd.none )

        Just (Route.Tree _) ->
            ( { model | currentPage = NotFound }, Cmd.none )

        Just (Route.UncachedActionResult _) ->
            ( { model | currentPage = NotFound }, Cmd.none )

        Just Route.Welcome ->
            ( { model | currentPage = Welcome }, Cmd.none )



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUrl url ->
            changeRouteTo (Route.fromUrl url) model

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.navigationKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )



-- VIEW


viewPage : Page.Page Msg -> Browser.Document Msg
viewPage contents =
    Browser.Document ("Buildbarn Browser - " ++ contents.title)
        [ Html.nav
            [ class "navbar"
            , class "navbar-dark"
            , class ("bg-" ++ contents.bannerColor)
            ]
            [ a [ class "navbar-brand", href "#" ] [ text "Buildbarn Browser" ] ]
        , Grid.container [] ([ h1 [] [ text contents.title ] ] ++ contents.body)
        ]


view : Model -> Browser.Document Msg
view model =
    case model.currentPage of
        NotFound ->
            viewPage Page.NotFound.view

        Welcome ->
            viewPage Page.Welcome.view



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
