module Page.Directory exposing (Model, Msg, init, update, view)

import Bytes
import Bytes.Decode
import Html exposing (p, text)
import Http
import Page
import Route
import Url.Builder



-- MODEL


type Model
    = Loading
    | Success


init : Route.Digest -> ( Model, Cmd Msg )
init digest =
    ( Loading
    , Http.get
        { url =
            Url.Builder.absolute
                [ "file"
                , digest.instance
                , digest.hash
                , String.fromInt digest.sizeBytes
                , "_"
                ]
                []
        , expect = Http.expectBytes GotDirectory directoryDecoder
        }
    )



-- TODO: Use a proper decoder.


directoryDecoder : Bytes.Decode.Decoder Int
directoryDecoder =
    Bytes.Decode.unsignedInt8



-- UPDATE


type Msg
    = GotDirectory (Result Http.Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Page.Page msg
view =
    { title = "Input directory"
    , bannerColor = "secondary"
    , body = []
    }
