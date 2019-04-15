module Buildbarn.Browser.Frontend.Page exposing
    ( Page
    , viewApiCallResult
    , viewCommandInfo
    , viewDirectory
    , viewDirectoryListing
    , viewDirectoryListingEntry
    , viewPage
    )

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing exposing (mb5, my4)
import Browser
import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Shell as Shell
import Html exposing (a, b, br, div, h1, p, table, td, text, th, tr)
import Html.Attributes exposing (class, href, style)
import Http
import Url.Builder


type alias Page msg =
    { title : String
    , bannerColor : String
    , body : List (Html.Html msg)
    }


viewPage : Page msg -> Browser.Document msg
viewPage contents =
    Browser.Document ("Buildbarn Browser - " ++ contents.title)
        [ Html.nav
            [ class "navbar"
            , class "navbar-dark"
            , class ("bg-" ++ contents.bannerColor)
            ]
            [ a [ class "navbar-brand", href "#" ] [ text "Buildbarn Browser" ] ]
        , Grid.container [ mb5 ] ([ h1 [ my4 ] [ text contents.title ] ] ++ contents.body)
        ]


viewCommandInfo : REv2.Command -> Html.Html msg
viewCommandInfo command =
    table [ class "table", style "table-layout" "fixed" ] <|
        [ tr []
            [ th [ style "width" "25%" ] [ text "Arguments:" ]
            , td [ class "text-monospace", style "width" "75%", style "overflow-x" "scroll" ] <|
                case command.arguments |> List.map viewShell of
                    first :: rest ->
                        [ div [ style "padding-left" "2em", style "text-indent" "-2em" ] <|
                            b [] [ text first ]
                                :: List.concatMap
                                    (\argument ->
                                        [ text " "
                                        , text argument
                                        ]
                                    )
                                    rest
                        ]

                    [] ->
                        []
            ]
        , tr []
            [ th [ style "width" "25%" ] [ text "Environment variables:" ]
            , command.environmentVariables
                |> List.map
                    (\(REv2.Command_EnvironmentVariableMessage env) ->
                        [ b [] [ text env.name ]
                        , text "="
                        , text <| viewShell env.value
                        ]
                    )
                |> List.intersperse [ br [] [] ]
                |> List.concat
                |> td [ class "text-monospace", style "width" "75%", style "overflow-x" "scroll" ]
            ]
        , tr []
            [ th [ style "width" "25%" ] [ text "Working directory:" ]
            , td [ class "text-monospace", style "width" "75%" ]
                [ text <|
                    if String.isEmpty command.workingDirectory then
                        "."

                    else
                        command.workingDirectory
                ]
            ]
        ]
            ++ (case command.platform of
                    Just (REv2.PlatformMessage platform) ->
                        [ th [ style "width" "25%" ] [ text "Platform properties:" ]
                        , platform.properties
                            |> List.map
                                (\(REv2.Platform_PropertyMessage property) ->
                                    [ b [] [ text property.name ]
                                    , text " = "
                                    , text property.value
                                    ]
                                )
                            |> List.intersperse [ br [] [] ]
                            |> List.concat
                            |> td [ style "width" "75%" ]
                        ]

                    Nothing ->
                        []
               )


viewDirectory : Api.Digest -> REv2.Directory -> List (Html.Html msg)
viewDirectory digest directory =
    [ viewDirectoryListing <|
        List.map
            (\(REv2.DirectoryNodeMessage entry) ->
                viewDirectoryListingEntry
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
            directory.directories
            ++ List.map
                (\(REv2.FileNodeMessage entry) ->
                    viewDirectoryListingEntry
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
                directory.files
            ++ List.map
                (\(REv2.SymlinkNodeMessage entry) ->
                    viewDirectoryListingEntry
                        "lrwxrwxrwx"
                        Nothing
                        [ text entry.name
                        , text " → "
                        , text entry.target
                        ]
                )
                directory.symlinks
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


viewDirectoryListingEntry : String -> Maybe REv2.DigestMessage -> List (Html.Html msg) -> Table.Row msg
viewDirectoryListingEntry permissions digest filename =
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


viewDirectoryListing : List (Table.Row msg) -> Html.Html msg
viewDirectoryListing entries =
    Table.simpleTable
        ( Table.simpleThead
            [ Table.th [] [ text "Mode" ]
            , Table.th [] [ text "Size" ]
            , Table.th [ Table.cellAttr <| style "width" "100%" ] [ text "Filename" ]
            ]
        , Table.tbody [] entries
        )


{-| Displays a message that the page is still loading, or that a HTTP
error occurred loading it.
-}
viewApiCallResult : Maybe (Api.CallResult a) -> (a -> List (Html.Html msg)) -> List (Html.Html msg)
viewApiCallResult result display =
    case result of
        Nothing ->
            [ p [] [ text "Loading..." ] ]

        Just (Err error) ->
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

        Just (Ok message) ->
            display message


viewShell : String -> String
viewShell s =
    s |> Shell.quote |> String.replace "-" "‑"
