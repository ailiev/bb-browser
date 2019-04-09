module Buildbarn.Browser.Frontend.Page.Command exposing (Model, Msg, init, update, view)

import Bootstrap.Utilities.Spacing exposing (my4)
import Build.Bazel.Remote.Execution.V2.Remote_execution as Remote_execution
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Page as Page
import Buildbarn.Browser.Frontend.Route as Route
import Html exposing (b, br, h2, table, td, text, th, tr)
import Html.Attributes exposing (class, style)
import Http



-- MODEL


type Model
    = Failure Http.Error
    | Loading
    | Success Remote_execution.Command


init : Route.Digest -> ( Model, Cmd Msg )
init digest =
    ( Loading
    , Api.getMessage "command" GotCommand Remote_execution.commandDecoder digest
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
    , body =
        case model of
            Failure error ->
                Page.viewError error

            Loading ->
                Page.viewLoading

            Success command ->
                [ table [ class "table", style "table-layout" "fixed" ] <|
                    [ tr []
                        [ th [ style "width" "25%" ] [ text "Arguments:" ]
                        , td [ class "text-monospace", style "width" "75%", style "overflow-x" "scroll" ] <|
                            case command.arguments of
                                first :: rest ->
                                    b [] [ text first ]
                                        :: List.concatMap
                                            (\argument ->
                                                [ text " "
                                                , text argument
                                                ]
                                            )
                                            rest

                                [] ->
                                    []
                        ]
                    , tr []
                        [ th [ style "width" "25%" ] [ text "Environment variables:" ]
                        , command.environmentVariables
                            |> List.map
                                (\(Remote_execution.Command_EnvironmentVariableMessage env) ->
                                    [ b [] [ text env.name ]
                                    , text "="
                                    , text env.value
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
                                Just (Remote_execution.PlatformMessage platform) ->
                                    [ th [ style "width" "25%" ] [ text "Environment variables:" ]
                                    , platform.properties
                                        |> List.map
                                            (\(Remote_execution.Platform_PropertyMessage property) ->
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
                ]
    }
