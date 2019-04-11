module Buildbarn.Browser.Frontend.Page.Action exposing (Model, Msg, init, update, view)

import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Page as Page
import Buildbarn.Browser.Frontend.Route as Route
import Html exposing (p, text)
import Http



-- MODEL


type Model
    = Loading


init : Route.Digest -> ( Model, Cmd Msg )
init digest =
    ( Loading
    , Cmd.batch
        [ Api.getMessage "action" GotAction REv2.actionDecoder digest
        , Api.getMessage "action_result" GotActionResult REv2.actionResultDecoder digest
        ]
    )



-- UPDATE


type Msg
    = GotAction (Result Http.Error REv2.Action)
    | GotActionResult (Result Http.Error REv2.ActionResult)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Page.Page msg
view model =
    { title = "Action"
    , bannerColor = "secondary"
    , body = []
    }
