module Buildbarn.Browser.Frontend.Route exposing (Route(..), fromUrl)

import Buildbarn.Browser.Frontend.Api as Api
import Url
import Url.Parser as Parser exposing ((</>))


digestParser : Parser.Parser (Api.Digest -> a) a
digestParser =
    Parser.map Api.Digest (Parser.string </> Parser.string </> Parser.int)


type Route
    = Action Api.Digest
    | Command Api.Digest
    | Directory Api.Digest
    | Tree Api.Digest (List String)
    | UncachedActionResult Api.Digest
    | Welcome


parser : Parser.Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Action (Parser.s "action" </> digestParser)
        , Parser.map Command (Parser.s "command" </> digestParser)
        , Parser.map Directory (Parser.s "directory" </> digestParser)
        , Parser.map Tree (Parser.s "tree" </> digestParser </> Parser.remainder)
        , Parser.map UncachedActionResult (Parser.s "uncached_action_result" </> digestParser)
        , Parser.map Welcome Parser.top
        ]


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser
