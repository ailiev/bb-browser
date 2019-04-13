module Buildbarn.Browser.Frontend.Page.Tree exposing (Model, Msg, init, update, view)

import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Page as Page
import Html exposing (p, text)
import Http
import Json.Decode as JD



-- MODEL


type alias TreeResult =
    Api.CallResult
        { tree : REv2.Tree
        , path : List String
        }


type alias Model =
    Maybe TreeResult


init : Api.Digest -> List String -> ( Model, Cmd Msg )
init digest path =
    ( Nothing
    , Api.getMessage
        "tree"
        GotTree
        (JD.map
            (\tree -> { tree = tree, path = path })
            REv2.treeDecoder
        )
        digest
    )



-- UPDATE


type Msg
    = GotTree TreeResult


update : Msg -> Model -> ( Model, Cmd Msg )
update (GotTree treeResult) model =
    ( Just treeResult, Cmd.none )



-- VIEW


view : Model -> Page.Page msg
view model =
    { title = "Output directory"
    , bannerColor = "secondary"
    , body =
        Page.viewApiCallResult model <|
            \message ->
                List.map (\filename -> p [] [ text filename ]) message.path
    }
