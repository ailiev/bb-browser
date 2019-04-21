module Buildbarn.Browser.Frontend.Page.Tree exposing (Model, Msg, init, update, view)

import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Api as Api
import Buildbarn.Browser.Frontend.Digest exposing (Digest)
import Buildbarn.Browser.Frontend.Page as Page
import Html exposing (p, text)
import Http
import Json.Decode as JD



-- MODEL


type alias Model =
    { tree : Maybe (Api.CallResult REv2.Tree)
    , path : List String
    }


init : Digest -> List String -> ( Model, Cmd Msg )
init digest path =
    ( { tree = Nothing, path = path }
    , Api.getMessage
        "tree"
        GotTree
        REv2.treeDecoder
        digest
    )



-- UPDATE


type Msg
    = GotTree Digest (Api.CallResult REv2.Tree)


update : Msg -> Model -> ( Model, Cmd Msg )
update (GotTree _ treeResult) model =
    ( { model | tree = Just treeResult }, Cmd.none )



-- VIEW


view : Model -> Page.Page msg
view model =
    { title = "Output directory"
    , bannerColor = "secondary"
    , body =
        List.map (\filename -> p [] [ text filename ]) model.path
    }
