module Buildbarn.Browser.Frontend.Page.Action exposing (Model, Msg, init, update, view)

import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Page as Page
import Buildbarn.Browser.Frontend.Route as Route
import Html exposing (h2, p, table, td, text, th, tr)
import Html.Attributes exposing (class, style)
import Http



-- MODEL


type alias Model =
    { action : Maybe (Api.CallResult REv2.Action)
    , actionResult : Maybe (Api.CallResult REv2.ActionResult)
    }


init : Route.Digest -> ( Model, Cmd Msg )
init digest =
    ( { action = Nothing, actionResult = Nothing }
    , Cmd.batch
        [ Api.getMessage "action" GotAction REv2.actionDecoder digest
        , Api.getMessage "action_result" GotActionResult REv2.actionResultDecoder digest
        ]
    )



-- UPDATE


type Msg
    = GotAction (Result Http.Error REv2.Action)
    | GotActionResult (Result Http.Error REv2.ActionResult)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAction action ->
            ( { model | action = Just action }, Cmd.none )

        GotActionResult actionResult ->
            ( { model | actionResult = Just actionResult }, Cmd.none )



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
        (Page.viewApiCallResult model.action <| \_ -> [])
            ++ [ h2 [] [ text "Result " ] ]
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
    }
