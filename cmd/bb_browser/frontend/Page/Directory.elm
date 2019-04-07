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
    = Failure Http.Error
    | Loading
    | Success Remote_execution.Directory


init : Route.Digest -> ( Model, Cmd Msg )
init digest =
    ( Loading
    , Http.get
        { url =
            Url.Builder.absolute
                [ "api"
                , "get_directory"
                ]
                [ Url.Builder.string "instance" digest.instance
                , Url.Builder.string "hash" digest.hash
                , Url.Builder.int "size_bytes" digest.sizeBytes
                ]
        , expect = Http.expectJson GotDirectory Remote_execution.directoryDecoder
        }
    )



-- UPDATE


type Msg
    = GotDirectory (Result Http.Error Remote_execution.Directory)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDirectory result ->
            case result of
                Ok directory ->
                    ( Success directory, Cmd.none )

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
                [ p []
                    [ text
                        (case error of
                            Http.BadUrl message ->
                                "BadURL " ++ message

                            Http.Timeout ->
                                "Timeout"

                            Http.NetworkError ->
                                "Network error"

                            Http.BadStatus code ->
                                "BadCode " ++ String.fromInt code

                            Http.BadBody message ->
                                "BadBody " ++ message
                        )
                    ]
                ]

            Loading ->
                [ p [] [ text "Loading..." ] ]

            Success directory ->
                directory.directories
                    |> List.map
                        (\(Remote_execution.DirectoryNodeMessage entry) ->
                            p [] [ text entry.name ]
                        )
    }
