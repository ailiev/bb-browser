module Page.Directory exposing (Model, Msg, init, update, view)

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
    | Success String Remote_execution.Directory


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
        , expect = Http.expectJson (GotDirectory digest.instance) Remote_execution.directoryDecoder
        }
    )



-- UPDATE


type Msg
    = GotDirectory String (Result Http.Error Remote_execution.Directory)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDirectory instance result ->
            case result of
                Ok directory ->
                    ( Success instance directory, Cmd.none )

                Err error ->
                    ( Failure error, Cmd.none )



-- VIEW


viewDirectoryEntry : String -> Maybe Remote_execution.DigestMessage -> List (Html.Html msg) -> Table.Row msg
viewDirectoryEntry permissions digest filename =
    Table.tr [ Table.rowAttr <| class "text-monospace" ]
        [ Table.td [] [ text permissions ]
        , Table.td [ Table.cellAttr <| style "text-align" "right" ]
            [ text
                (case digest of
                    Nothing ->
                        ""

                    Just (Remote_execution.DigestMessage d) ->
                        String.fromInt d.sizeBytes
                )
            ]
        , Table.td [] filename
        ]


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

            Success instance directory ->
                [ Table.simpleTable
                    ( Table.simpleThead
                        [ Table.th [] [ text "Mode" ]
                        , Table.th [] [ text "Size" ]
                        , Table.th [ Table.cellAttr <| style "width" "100%" ] [ text "Filename" ]
                        ]
                    , Table.tbody [] <|
                        (directory.directories
                            |> List.map
                                (\(Remote_execution.DirectoryNodeMessage entry) ->
                                    viewDirectoryEntry
                                        "drwxr‑xr‑x"
                                        entry.digest
                                        [ case entry.digest of
                                            Nothing ->
                                                text entry.name

                                            Just (Remote_execution.DigestMessage d) ->
                                                a
                                                    [ href <|
                                                        "#directory/"
                                                            ++ instance
                                                            ++ "/"
                                                            ++ d.hash
                                                            ++ "/"
                                                            ++ String.fromInt d.sizeBytes
                                                    ]
                                                    [ text entry.name ]
                                        , text "/"
                                        ]
                                )
                        )
                            ++ (directory.symlinks
                                    |> List.map
                                        (\(Remote_execution.SymlinkNodeMessage entry) ->
                                            viewDirectoryEntry
                                                "lrwxrwxrwx"
                                                Nothing
                                                [ text entry.name
                                                , text " → "
                                                , text entry.target
                                                ]
                                        )
                               )
                            ++ (directory.files
                                    |> List.map
                                        (\(Remote_execution.FileNodeMessage entry) ->
                                            viewDirectoryEntry
                                                (if entry.isExecutable then
                                                    "‑r‑xr‑xr‑x"

                                                 else
                                                    "‑r‑‑r‑‑r‑‑"
                                                )
                                                entry.digest
                                                [ case entry.digest of
                                                    Nothing ->
                                                        text entry.name

                                                    Just (Remote_execution.DigestMessage d) ->
                                                        a
                                                            [ href <|
                                                                Url.Builder.absolute
                                                                    [ "file"
                                                                    , instance
                                                                    , d.hash
                                                                    , String.fromInt d.sizeBytes
                                                                    , entry.name
                                                                    ]
                                                                    []
                                                            ]
                                                            [ text entry.name ]
                                                ]
                                        )
                               )
                    )
                ]
    }
