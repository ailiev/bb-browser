module Page.Directory exposing (Model, Msg, init, update, view)

import Build.Bazel.Remote.Execution.V2.Remote_execution as Remote_execution
import Bytes
import Bytes.Decode
import Html exposing (p, text)
import Http
import Page
import Route
import Url.Builder



-- MODEL


type Model
    = Loading
    | Success


init : Route.Digest -> ( Model, Cmd Msg )
init digest =
    ( Loading
    , Http.get
        { url =
            Url.Builder.absolute
                [ "file"
                , digest.instance
                , digest.hash
                , String.fromInt digest.sizeBytes
                , "_"
                ]
                []
        , expect = Http.expectJson GotDirectory Remote_execution.directoryDecoder
        }
    )



-- UPDATE


type Msg
    = GotDirectory (Result Http.Error Remote_execution.Directory)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Page.Page msg
view =
    { title = "Input directory"
    , bannerColor = "secondary"
    , body = []
    }
