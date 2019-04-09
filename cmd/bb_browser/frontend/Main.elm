module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing exposing (mb5, my4)
import Browser
import Browser.Navigation as Navigation
import Html exposing (a, h1, text)
import Html.Attributes exposing (class, href)
import Page
import Page.Command
import Page.Directory
import Page.NotFound
import Page.Welcome
import Platform.Cmd
import Platform.Sub
import Route
import Url exposing (Url)



-- MODEL


type CurrentPage
    = Command Page.Command.Model
    | Directory Page.Directory.Model
    | NotFound
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

        Just (Route.Command digest) ->
            Page.Command.init digest
                |> updateWith Command GotCommandMsg model

        Just (Route.Directory digest) ->
            Page.Directory.init digest
                |> updateWith Directory GotDirectoryMsg model

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
    | GotCommandMsg Page.Command.Msg
    | GotDirectoryMsg Page.Directory.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.currentPage ) of
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    if url.path /= "" then
                        -- Internal link, but not to this application.
                        ( model
                        , Navigation.load <| Url.toString url
                        )

                    else
                        -- Internal link inside this application.
                        ( model
                        , Navigation.pushUrl model.navigationKey (Url.toString url)
                        )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        ( GotCommandMsg subMsg, Command subModel ) ->
            Page.Command.update subMsg subModel
                |> updateWith Command GotCommandMsg model

        ( GotDirectoryMsg subMsg, Directory subModel ) ->
            Page.Directory.update subMsg subModel
                |> updateWith Directory GotDirectoryMsg model

        -- Ignore invalid message/model pairs.
        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> CurrentPage) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | currentPage = toModel subModel }
    , Cmd.map toMsg subCmd
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
        , Grid.container [ mb5 ] ([ h1 [ my4 ] [ text contents.title ] ] ++ contents.body)
        ]


view : Model -> Browser.Document Msg
view model =
    case model.currentPage of
        Command subModel ->
            viewPage <| Page.Command.view subModel

        Directory subModel ->
            viewPage <| Page.Directory.view subModel

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
