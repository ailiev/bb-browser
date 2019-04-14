module Buildbarn.Browser.Frontend.Page.Action exposing (Model, Msg, init, update, view)

import Bootstrap.Utilities.Spacing exposing (my4)
import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Page as Page
import Google.Protobuf.Duration as Duration
import Html exposing (a, h2, p, sup, table, td, text, th, tr)
import Html.Attributes exposing (class, href, style)
import Http
import Json.Decode as JD



-- MODEL


type alias Model =
    { action : Maybe (Api.CallResult ActionModel)
    , actionResult : Maybe (Api.CallResult REv2.ActionResult)
    }


type alias ActionModel =
    { data : REv2.Action
    , command : Maybe (Api.CallResult REv2.Command)
    , inputRoot : Maybe (Api.CallResult REv2.Directory)
    }


init : Api.Digest -> ( Model, Cmd Msg )
init digest =
    ( { action = Nothing, actionResult = Nothing }
    , Cmd.batch
        [ Api.getMessage "action" (GotAction digest) REv2.actionDecoder digest
        , Api.getMessage "action_result" GotActionResult REv2.actionResultDecoder digest
        ]
    )



-- UPDATE


type Msg
    = GotAction Api.Digest (Api.CallResult REv2.Action)
    | GotActionResult (Api.CallResult REv2.ActionResult)
    | GotCommand (Api.CallResult REv2.Command)
    | GotInputRoot (Api.CallResult REv2.Directory)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAction actionDigest action ->
            ( model
                |> (mapFieldAction <|
                        \_ ->
                            Just
                                (Result.map
                                    (\data ->
                                        { data = data
                                        , command = Nothing
                                        , inputRoot = Nothing
                                        }
                                    )
                                    action
                                )
                   )
            , Cmd.batch
                [ Api.getChildMessage
                    "command"
                    GotCommand
                    REv2.commandDecoder
                    (\actionMessage -> actionMessage.commandDigest)
                    actionDigest
                    action
                , Api.getChildMessage
                    "directory"
                    GotInputRoot
                    REv2.directoryDecoder
                    (\actionMessage -> actionMessage.inputRootDigest)
                    actionDigest
                    action
                ]
            )

        GotActionResult actionResult ->
            ( model
                |> (mapFieldActionResult <|
                        \_ -> Just actionResult
                   )
            , Cmd.none
            )

        GotCommand command ->
            ( model
                |> (mapFieldAction <|
                        Maybe.map <|
                            Result.map <|
                                mapFieldCommand <|
                                    \_ -> Just command
                   )
            , Cmd.none
            )

        GotInputRoot directory ->
            ( model
                |> (mapFieldAction <|
                        Maybe.map <|
                            Result.map <|
                                mapFieldInputRoot <|
                                    \_ -> Just directory
                   )
            , Cmd.none
            )


mapFieldAction updater record =
    { record | action = updater record.action }


mapFieldActionResult updater record =
    { record | actionResult = updater record.action }


mapFieldCommand updater record =
    { record | command = updater record.command }


mapFieldInputRoot updater record =
    { record | inputRoot = updater record.inputRoot }



-- VIEW


view : Model -> Page.Page msg
view model =
    { title = "Action"
    , bannerColor =
        case model.actionResult of
            Just (Ok actionResult) ->
                if actionResult.exitCode == 0 then
                    "success"

                else
                    "danger"

            _ ->
                "secondary"
    , body =
        (Page.viewApiCallResult model.action <|
            \actionModel ->
                [ table [ class "table", style "table-layout" "fixed" ] <|
                    [ tr []
                        [ th [ style "width" "25%" ] [ text "Timeout:" ]
                        , td [ style "width" "75%" ] <|
                            case actionModel.data.timeout of
                                Just (Duration.DurationMessage timeout) ->
                                    [ text <| String.fromInt timeout.seconds
                                    , text " seconds "
                                    ]

                                _ ->
                                    [ text "∞" ]
                        ]
                    , tr []
                        [ th [ style "width" "25%" ] [ text "Do not cache:" ]
                        , td [ style "width" "75%" ]
                            [ text <|
                                if actionModel.data.doNotCache then
                                    "yes"

                                else
                                    "no"
                            ]
                        ]
                    ]
                , h2 [ my4 ]
                    [ text "Command"
                    , sup [] [ a [ href "#" ] [ text "*" ] ]
                    ]
                ]
                    ++ (Page.viewApiCallResult actionModel.command <|
                            \command -> [ Page.viewCommandInfo command ]
                       )
        )
            ++ [ h2 [ my4 ] [ text "Result " ] ]
            ++ (Page.viewApiCallResult model.actionResult <|
                    \actionResult ->
                        [ table [ class "table", style "table-layout" "fixed" ] <|
                            [ tr []
                                [ th [ style "width" "25%" ] [ text "Exit code:" ]
                                , td [ style "width" "75%" ] [ text <| String.fromInt actionResult.exitCode ]
                                ]
                            ]
                        ]
               )
            ++ [ h2 [ my4 ]
                    [ text "Input files"
                    , sup [] [ a [ href "#" ] [ text "*" ] ]
                    ]
               ]
            ++ (Page.viewApiCallResult model.action <|
                    \action ->
                        Page.viewApiCallResult action.inputRoot <|
                            -- TODO: Use the right digest.
                            Page.viewDirectory { instance = "", hash = "", sizeBytes = 0 }
               )
            ++ [ h2 [ my4 ] [ text "Output files " ] ]
    }
