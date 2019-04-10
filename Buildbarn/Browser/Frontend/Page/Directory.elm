module Buildbarn.Browser.Frontend.Page.Directory exposing (Model, Msg, init, update, view)

import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Page as Page
import Buildbarn.Browser.Frontend.Route as Route
import Html exposing (a, text)
import Html.Attributes exposing (class, href, style)
import Http
import Url.Builder



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


viewDirectoryEntry : String -> Maybe REv2.DigestMessage -> List (Html.Html msg) -> Table.Row msg
viewDirectoryEntry permissions digest filename =
    Table.tr [ Table.rowAttr <| class "text-monospace" ]
        [ Table.td [] [ text permissions ]
        , Table.td [ Table.cellAttr <| style "text-align" "right" ]
            (case digest of
                Nothing ->
                    []

                Just (REv2.DigestMessage d) ->
                    [ text <| String.fromInt d.sizeBytes ]
            )
        , Table.td [] filename
        ]


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
                [ Table.simpleTable
                    ( Table.simpleThead
                        [ Table.th [] [ text "Mode" ]
                        , Table.th [] [ text "Size" ]
                        , Table.th [ Table.cellAttr <| style "width" "100%" ] [ text "Filename" ]
                        ]
                    , Table.tbody [] <|
                        (directory.directories
                            |> List.map
                                (\(REv2.DirectoryNodeMessage entry) ->
                                    viewDirectoryEntry
                                        "drwxr‑xr‑x"
                                        entry.digest
                                        [ case entry.digest of
                                            Nothing ->
                                                text entry.name

                                            Just (REv2.DigestMessage childDigest) ->
                                                a
                                                    [ href <|
                                                        "#directory/"
                                                            ++ digest.instance
                                                            ++ "/"
                                                            ++ childDigest.hash
                                                            ++ "/"
                                                            ++ String.fromInt childDigest.sizeBytes
                                                    ]
                                                    [ text entry.name ]
                                        , text "/"
                                        ]
                                )
                        )
                            ++ (directory.symlinks
                                    |> List.map
                                        (\(REv2.SymlinkNodeMessage entry) ->
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
                                        (\(REv2.FileNodeMessage entry) ->
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

                                                    Just (REv2.DigestMessage childDigest) ->
                                                        a
                                                            [ href <|
                                                                Url.Builder.absolute
                                                                    [ "file"
                                                                    , digest.instance
                                                                    , childDigest.hash
                                                                    , String.fromInt childDigest.sizeBytes
                                                                    , entry.name
                                                                    ]
                                                                    []
                                                            ]
                                                            [ text entry.name ]
                                                ]
                                        )
                               )
                    )
                , Button.linkButton
                    [ Button.primary
                    , Button.attrs
                        [ href <|
                            Url.Builder.absolute
                                [ "directory"
                                , digest.instance
                                , digest.hash
                                , String.fromInt digest.sizeBytes
                                , "/"
                                ]
                                [ Url.Builder.string "format" "tar" ]
                        ]
                    ]
                    [ text "Download as tarball" ]
                ]
    }
