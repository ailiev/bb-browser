module Buildbarn.Browser.Frontend.Page.Command exposing (Model, Msg, init, update, view)

import Bootstrap.Utilities.Spacing exposing (my4)
import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Page as Page
import Buildbarn.Browser.Frontend.Route as Route
import Html exposing (b, br, div, h2, table, td, text, th, tr)
import Html.Attributes exposing (class, style)
import Http



-- MODEL


type alias CommandResult =
    Api.CallResult REv2.Command


type alias Model =
    Maybe CommandResult


init : Route.Digest -> ( Model, Cmd Msg )
init digest =
    ( Nothing
    , Api.getMessage "command" GotCommand REv2.commandDecoder digest
    )



-- UPDATE


type Msg
    = GotCommand CommandResult


update : Msg -> Model -> ( Model, Cmd Msg )
update (GotCommand commandResult) model =
    ( Just commandResult, Cmd.none )



-- VIEW


view : Model -> Page.Page msg
view model =
    { title = "Command"
    , bannerColor = "secondary"
    , body =
        Page.viewApiCallResult model <|
            \command ->
                [ table [ class "table", style "table-layout" "fixed" ] <|
                    [ tr []
                        [ th [ style "width" "25%" ] [ text "Arguments:" ]
                        , td [ class "text-monospace", style "width" "75%", style "overflow-x" "scroll" ] <|
                            case command.arguments |> List.map Page.viewShell of
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
                                    , text <| Page.viewShell env.value
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
                , h2 [ my4 ] [ text "Output files" ]
                , Page.viewDirectoryListing <|
                    List.map
                        (\path ->
                            Page.viewDirectoryListingEntry
                                "drwxr‑xr‑x"
                                Nothing
                                [ text path, text "/" ]
                        )
                        command.outputDirectories
                        ++ List.map
                            (\path ->
                                Page.viewDirectoryListingEntry
                                    "‑rw‑r‑‑r‑‑"
                                    Nothing
                                    [ text path ]
                            )
                            command.outputFiles
                ]
    }
