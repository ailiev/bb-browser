module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Page.NotFound as NotFound
import Page.Welcome as Welcome
import Platform.Cmd
import Platform.Sub
import Url exposing (Url)



-- MODEL


type Model
    = NotFound NotFound.Model
    | Welcome Welcome.Model


type alias Flags =
    {}


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
    ( NotFound {}, Cmd.none )



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document "This is the title"
        [ div []
            [ button [] [ text "-" ]
            , div [] [ text "123" ]
            , button [] [ text "+" ]
            ]
        ]



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
