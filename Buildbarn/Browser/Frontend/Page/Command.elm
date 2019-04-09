module Buildbarn.Browser.Frontend.Page.Command exposing (Model, Msg, init, update, view)

import Build.Bazel.Remote.Execution.V2.Remote_execution as Remote_execution
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Page as Page
import Buildbarn.Browser.Frontend.Route as Route
import Http



-- MODEL


type Model
    = Failure Http.Error
    | Loading
    | Success Remote_execution.Command


init : Route.Digest -> ( Model, Cmd Msg )
init digest =
    ( Loading
    , Api.getMessage "command" GotCommand Remote_execution.commandDecoder digest
    )



-- UPDATE


type Msg
    = GotCommand (Result Http.Error Remote_execution.Command)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCommand result ->
            case result of
                Ok command ->
                    ( Success command, Cmd.none )

                Err error ->
                    ( Failure error, Cmd.none )



-- VIEW


view : Model -> Page.Page msg
view model =
    { title = "Command"
    , bannerColor = "secondary"
    , body =
        case model of
            Failure error ->
                Page.viewError error

            Loading ->
                Page.viewLoading

            Success _ ->
                []
    }
