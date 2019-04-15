module Buildbarn.Browser.Frontend.Api exposing
    ( CallResult
    , Digest
    , getChildMessage
    , getMessage
    )

import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Http
import Json.Decode as JD
import Url.Builder


type alias Digest =
    { instance : String
    , hash : String
    , sizeBytes : Int
    }


type alias CallResult message =
    Result Http.Error message


getMessage : String -> (CallResult a -> msg) -> JD.Decoder a -> Digest -> Cmd msg
getMessage endpoint toMsg decoder digest =
    Http.get
        { url =
            Url.Builder.relative
                [ "api", "get_" ++ endpoint ]
                [ Url.Builder.string "instance" digest.instance
                , Url.Builder.string "hash" digest.hash
                , Url.Builder.int "size_bytes" digest.sizeBytes
                ]
        , expect = Http.expectJson toMsg decoder
        }


getChildMessage : String -> (Digest -> CallResult a -> msg) -> JD.Decoder a -> (b -> Maybe REv2.DigestMessage) -> Digest -> CallResult b -> Cmd msg
getChildMessage endpoint toMsg decoder getChildDigest parentDigest parentResult =
    case parentResult of
        Ok parent ->
            case getChildDigest parent of
                Just (REv2.DigestMessage childDigest) ->
                    let
                        newDigest =
                            { instance = parentDigest.instance
                            , hash = childDigest.hash
                            , sizeBytes = childDigest.sizeBytes
                            }
                    in
                    getMessage endpoint
                        (toMsg newDigest)
                        decoder
                        newDigest

                _ ->
                    Cmd.none

        _ ->
            Cmd.none
