module Buildbarn.Browser.Frontend.Page.Tree exposing (Model, Msg, init, update, view)

import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Page as Page
import Buildbarn.Browser.Frontend.Route as Route
import Html exposing (p, text)
import Http



-- MODEL


type Model
    = Failure Http.Error
    | Loading
    | Success REv2.Tree (List String)


init : Route.Digest -> (List String) -> ( Model, Cmd Msg )
init digest path =
    ( Loading
    , Api.getMessage "tree" (GotTree path) REv2.treeDecoder digest
    )



-- UPDATE


type Msg
    = GotTree (List String) (Result Http.Error REv2.Tree)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTree path result ->
            case result of
                Ok tree ->
                    ( Success tree path, Cmd.none )

                Err error ->
                    ( Failure error, Cmd.none )



-- VIEW


view : Model -> Page.Page msg
view model =
    { title = "Output directory"
    , bannerColor = "secondary"
    , body =
        case model of
            Failure error ->
                Page.viewError error

            Loading ->
                Page.viewLoading

            Success _ path ->
                List.map (\filename -> p [] [ text filename ]) path
    }
