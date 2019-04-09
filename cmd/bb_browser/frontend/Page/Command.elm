module Page.Command exposing (Model, Msg, init, update, view)

import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Build.Bazel.Remote.Execution.V2.Remote_execution as Remote_execution
import Bytes
import Bytes.Decode
import Html exposing (a, p, text)
import Html.Attributes exposing (class, href, style)
import Http
import Page
import Route
import Url.Builder



-- MODEL


type Model
    = Failure Http.Error
    | Loading
    | Success Remote_execution.Command


init : Route.Digest -> ( Model, Cmd Msg )
init digest =
    ( Loading
    , Http.get
        { url =
            Url.Builder.absolute
                [ "api"
                , "get_command"
                ]
                [ Url.Builder.string "instance" digest.instance
                , Url.Builder.string "hash" digest.hash
                , Url.Builder.int "size_bytes" digest.sizeBytes
                ]
        , expect = Http.expectJson GotCommand Remote_execution.commandDecoder
        }
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
    , body = []
    }
