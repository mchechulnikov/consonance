module Router exposing (..)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string)



type Route
    = RouteRoot
    | RouteNote String
    | RouteInterval String String
    | RouteChord String String
    | RouteScale String String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map RouteScale (s "scale" </> string </> string)
        , map RouteChord (s "chord" </> string </> string)
        , map RouteInterval (s "interval" </> string </> string)
        , map RouteNote (s "note" </> string)
        , map RouteRoot (s "")
        ]


parserUrl : Url -> Route
parserUrl url =
    url
        |> parse routeParser
        |> Maybe.withDefault RouteRoot


changeValue : Route -> String -> String
changeValue route value =
    let replaceSpaces = String.replace " " "-" in
    let note = Url.percentEncode value in
    case route of
        RouteRoot ->
            "/note/" ++ note

        RouteNote _ ->
            "/note/" ++ note

        RouteInterval intervalKind _ ->
            "/interval/" ++ (replaceSpaces intervalKind) ++ "/" ++ note

        RouteChord chordKind _ ->
            "/chord/" ++ (replaceSpaces chordKind) ++ "/" ++ note

        RouteScale scaleKind _ ->
            "/scale/" ++ (replaceSpaces scaleKind) ++ "/" ++ note

