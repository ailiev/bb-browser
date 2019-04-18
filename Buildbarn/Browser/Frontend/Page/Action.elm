module Buildbarn.Browser.Frontend.Page.Action exposing
    ( Model
    , Msg
    , initCached
    , initUncached
    , update
    , view
    )

import Bootstrap.Badge as Badge
import Bootstrap.Utilities.Spacing exposing (my4)
import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Digest exposing (Digest)
import Buildbarn.Browser.Frontend.Page as Page
import Bytes exposing (Bytes)
import Google.Protobuf.Duration as Duration
import Html exposing (Html, a, h2, p, sup, table, td, text, th, tr)
import Html.Attributes exposing (class, href, style)
import Http
import Json.Decode as JD
import Pkg.Proto.Cas.Cas as Cas
import Url.Builder



-- MODEL


type alias Model =
    { action : Maybe (Api.CallResult ActionModel)
    , actionResult : Maybe (Api.CallResult ActionResultModel)
    }


type alias ActionModel =
    { data : REv2.Action
    , command : Maybe (Api.CallResult REv2.Command)
    , inputRoot : Maybe (Api.CallResult REv2.Directory)
    }


type alias ActionResultModel =
    { data : REv2.ActionResult
    , stderr : Maybe (Result Http.Error String)
    , stdout : Maybe (Result Http.Error String)
    }


initCached : Digest -> ( Model, Cmd Msg )
initCached digest =
    ( { action = Nothing, actionResult = Nothing }
    , Cmd.batch
        [ Api.getMessage "action" GotAction REv2.actionDecoder digest
        , Api.getMessage "action_result" GotActionResult REv2.actionResultDecoder digest
        ]
    )


initUncached : Digest -> ( Model, Cmd Msg )
initUncached digest =
    ( { action = Nothing, actionResult = Nothing }
    , Api.getMessage "uncached_action_result" GotUncachedActionResult Cas.uncachedActionResultDecoder digest
    )



-- UPDATE


type Msg
    = GotAction Digest (Api.CallResult REv2.Action)
    | GotActionResult Digest (Api.CallResult REv2.ActionResult)
    | GotCommand Digest (Api.CallResult REv2.Command)
    | GotInputRoot Digest (Api.CallResult REv2.Directory)
    | GotStderr (Result Http.Error String)
    | GotStdout (Result Http.Error String)
    | GotUncachedActionResult Digest (Api.CallResult Cas.UncachedActionResult)


getCmdForStream : (Result Http.Error String -> msg) -> Bytes -> Maybe REv2.DigestMessage -> Cmd msg
getCmdForStream toMsg raw maybeDigest =
    if Bytes.width raw > 0 then
        Cmd.none

    else
        case maybeDigest of
            Just (REv2.DigestMessage digest) ->
                -- TODO: Add size limit.
                Http.get
                    { url =
                        Url.Builder.relative
                            [ "file "
                            , "TODO"
                            , digest.hash
                            , String.fromInt digest.sizeBytes
                            , "log.txt"
                            ]
                            []
                    , expect = Http.expectString toMsg
                    }

            _ ->
                Cmd.none


getCmdsForActionResult : REv2.ActionResult -> Cmd Msg
getCmdsForActionResult actionResult =
    Cmd.batch
        [ getCmdForStream GotStderr actionResult.stderrRaw actionResult.stderrDigest
        , getCmdForStream GotStdout actionResult.stdoutRaw actionResult.stdoutDigest
        ]


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

        GotActionResult _ actionResult ->
            ( model
                |> (mapFieldActionResult <|
                        \_ ->
                            Just <|
                                Result.map
                                    (\actionResultMessage ->
                                        { data = actionResultMessage
                                        , stdout = Nothing
                                        , stderr = Nothing
                                        }
                                    )
                                    actionResult
                   )
            , case actionResult of
                Ok actionResultMessage ->
                    getCmdsForActionResult actionResultMessage

                _ ->
                    Cmd.none
            )

        GotCommand _ command ->
            ( model
                |> (mapFieldAction <|
                        Maybe.map <|
                            Result.map <|
                                mapFieldCommand <|
                                    \_ -> Just command
                   )
            , Cmd.none
            )

        GotInputRoot _ directory ->
            ( model
                |> (mapFieldAction <|
                        Maybe.map <|
                            Result.map <|
                                mapFieldInputRoot <|
                                    \_ -> Just directory
                   )
            , Cmd.none
            )

        GotStderr body ->
            ( model
                |> (mapFieldActionResult <|
                        Maybe.map <|
                            Result.map <|
                                mapFieldStderr <|
                                    \a -> Just body
                   )
            , Cmd.none
            )

        GotStdout body ->
            ( model
                |> (mapFieldActionResult <|
                        Maybe.map <|
                            Result.map <|
                                mapFieldStdout <|
                                    \a -> Just body
                   )
            , Cmd.none
            )

        GotUncachedActionResult uncachedActionResultDigest uncachedActionResult ->
            ( case uncachedActionResult of
                Err e ->
                    { model | actionResult = Just (Err e) }

                Ok v ->
                    case v.actionResult of
                        Nothing ->
                            model

                        Just (REv2.ActionResultMessage actionResult) ->
                            { model
                                | actionResult =
                                    Just
                                        (Ok
                                            { data = actionResult
                                            , stderr = Nothing
                                            , stdout = Nothing
                                            }
                                        )
                            }
            , Api.getChildMessage
                "action"
                GotAction
                REv2.actionDecoder
                (\uncachedActionResultMessage -> uncachedActionResultMessage.actionDigest)
                uncachedActionResultDigest
                uncachedActionResult
            )


mapFieldAction : (a -> a) -> { b | action : a } -> { b | action : a }
mapFieldAction updater record =
    { record | action = updater record.action }


mapFieldActionResult : (a -> a) -> { b | actionResult : a } -> { b | actionResult : a }
mapFieldActionResult updater record =
    { record | actionResult = updater record.actionResult }


mapFieldCommand : (a -> a) -> { b | command : a } -> { b | command : a }
mapFieldCommand updater record =
    { record | command = updater record.command }


mapFieldInputRoot : (a -> a) -> { b | inputRoot : a } -> { b | inputRoot : a }
mapFieldInputRoot updater record =
    { record | inputRoot = updater record.inputRoot }


mapFieldStderr : (a -> a) -> { b | stderr : a } -> { b | stderr : a }
mapFieldStderr updater record =
    { record | stderr = updater record.stderr }


mapFieldStdout : (a -> a) -> { b | stdout : a } -> { b | stdout : a }
mapFieldStdout updater record =
    { record | stdout = updater record.stdout }



-- VIEW


view : Model -> Page.Page msg
view model =
    { title = "Action"
    , bannerColor =
        case model.actionResult of
            Just (Ok actionResult) ->
                if actionResult.data.exitCode == 0 then
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
                                , td [ style "width" "75%" ]
                                    [ text <| String.fromInt actionResult.data.exitCode
                                    , text " "
                                    , if actionResult.data.exitCode == 0 then
                                        Badge.badgeSuccess [] [ text "Success " ]

                                      else
                                        Badge.badgeDanger [] [ text "Failure " ]
                                    ]
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
            ++ (case
                    ( model.actionResult
                        |> Maybe.andThen Result.toMaybe
                    , model.action
                        |> Maybe.andThen Result.toMaybe
                        |> Maybe.andThen (\actionModel -> actionModel.command)
                        |> Maybe.andThen Result.toMaybe
                    )
                of
                    ( Nothing, Nothing ) ->
                        []

                    ( maybeActionResult, maybeCommand ) ->
                        [ h2 [ my4 ] [ text "Output files" ]
                        , Page.viewDirectoryListing <|
                            (case maybeActionResult of
                                Just actionResult ->
                                    List.map
                                        (\(REv2.OutputDirectoryMessage entry) ->
                                            Page.viewDirectoryListingEntry
                                                "drwxrwxrwx"
                                                entry.treeDigest
                                                [ text entry.path ]
                                        )
                                        actionResult.data.outputDirectories
                                        ++ List.map
                                            (\(REv2.OutputSymlinkMessage entry) ->
                                                Page.viewDirectoryListingEntry
                                                    "lrwxrwxrwx"
                                                    Nothing
                                                    [ text entry.path ]
                                            )
                                            actionResult.data.outputDirectorySymlinks
                                        ++ List.map
                                            (\(REv2.OutputFileMessage entry) ->
                                                Page.viewDirectoryListingEntry
                                                    (if entry.isExecutable then
                                                        "‑rwxr‑xr‑x"

                                                     else
                                                        "‑rw‑r‑‑r‑‑"
                                                    )
                                                    entry.digest
                                                    [ text entry.path ]
                                            )
                                            actionResult.data.outputFiles
                                        ++ List.map
                                            (\(REv2.OutputSymlinkMessage entry) ->
                                                Page.viewDirectoryListingEntry
                                                    "lrwxrwxrwx"
                                                    Nothing
                                                    [ text entry.path ]
                                            )
                                            actionResult.data.outputFileSymlinks

                                Nothing ->
                                    []
                            )
                                ++ (case maybeCommand of
                                        Just command ->
                                            []

                                        Nothing ->
                                            []
                                   )
                        ]
               )
    }
