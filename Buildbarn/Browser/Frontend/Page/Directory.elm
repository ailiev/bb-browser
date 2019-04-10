module Buildbarn.Browser.Frontend.Page.Directory exposing (Model, Msg, init, update, view)

import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Page as Page
import Buildbarn.Browser.Frontend.Route as Route
import Http



-- MODEL


type Model
    = Failure Http.Error
    | Loading
    | Success Route.Digest REv2.Directory


init : Route.Digest -> ( Model, Cmd Msg )
init digest =
    ( Loading
    , Api.getMessage "directory" (GotDirectory digest) REv2.directoryDecoder digest
    )



-- UPDATE


type Msg
    = GotDirectory Route.Digest (Result Http.Error REv2.Directory)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDirectory digest result ->
            case result of
                Ok directory ->
                    ( Success digest directory, Cmd.none )

                Err error ->
                    ( Failure error, Cmd.none )



-- VIEW


view : Model -> Page.Page msg
view model =
    { title = "Input directory"
    , bannerColor = "secondary"
    , body =
        case model of
            Failure error ->
                Page.viewError error

            Loading ->
                Page.viewLoading

            Success digest directory ->
                Page.viewDirectory digest directory
    }
