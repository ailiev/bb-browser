module Buildbarn.Browser.Frontend.Page.Directory exposing (Model, Msg, init, update, view)

import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Page as Page
import Http
import Json.Decode as JD



-- MODEL


type alias DirectoryResult =
    Api.CallResult
        { digest : Api.Digest
        , directory : REv2.Directory
        }


type alias Model =
    Maybe DirectoryResult


init : Api.Digest -> ( Model, Cmd Msg )
init digest =
    ( Nothing
    , Api.getMessage
        "directory"
        GotDirectory
        (JD.map
            (\directory -> { digest = digest, directory = directory })
            REv2.directoryDecoder
        )
        digest
    )



-- UPDATE


type Msg
    = GotDirectory DirectoryResult


update : Msg -> Model -> ( Model, Cmd Msg )
update (GotDirectory directoryResult) model =
    ( Just directoryResult, Cmd.none )



-- VIEW


view : Model -> Page.Page msg
view model =
    { title = "Input directory"
    , bannerColor = "secondary"
    , body =
        Page.viewApiCallResult model <|
            \message -> Page.viewDirectory message.digest message.directory
    }
