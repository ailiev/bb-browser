module Buildbarn.Browser.Frontend.Page exposing
    ( Page
    , viewDirectory
    , viewError
    , viewLoading
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
import Buildbarn.Browser.Frontend.Route as Route
import Html exposing (a, h1, p, text)
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


viewDirectory : Route.Digest -> REv2.Directory -> List (Html.Html msg)
viewDirectory digest directory =
    [ viewDirectoryListing <|
        (directory.directories
            |> List.map
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
        )
            ++ (directory.symlinks
                    |> List.map
                        (\(REv2.SymlinkNodeMessage entry) ->
                            viewDirectoryListingEntry
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


{-| Displays a HTTP error message in a friendly way.
-}
viewError : Http.Error -> List (Html.Html msg)
viewError error =
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


{-| Displays a message that the page is still loading.
-}
viewLoading : List (Html.Html msg)
viewLoading =
    [ p [] [ text "Loading..." ] ]
