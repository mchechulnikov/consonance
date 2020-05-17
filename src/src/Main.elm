port module Main exposing (..)

import Chord exposing (Chord(..))
import Fretboard
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Guitar exposing (FretPoint, Guitar, GuitarFret(..), GuitarString(..))
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Styled
import List exposing (singleton)
import Note exposing (Base(..), Octave(..))
import Router exposing (Route(..))
import Scale exposing (Scale(..))
import Url exposing (Url)



-- PORTS


port localStorageGetItem : String -> Cmd msg
port localStorageSetItem : (String, String) -> Cmd msg
port localStorageGetItemResponse : ((String, String) -> msg) -> Sub msg



-- MAIN


main =
    Browser.application
        { init = init
        , view = document
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


document model =
    { title = "Consonance | Guitar fretboard assistant"
    , body = [ view model ]
    }


init : () -> Url -> Key -> (Model, Cmd msg)
init _ url key =
    let guitar = Guitar.new Guitar.standardTuning 22 in
    let route = Router.parserUrl url in
    ( { fretboard =
        { guitar = guitar
        , selectedFretPoints =
            selectFretPoints guitar route
        }
      , navKey = key
      , route = route
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { fretboard : Fretboard.Model
    , navKey : Nav.Key
    , route : Route
    }



-- UPDATE


type Msg
  = Idle
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url
  | FretboardMsg Fretboard.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let fretboard = model.fretboard in
    case msg of
        Idle ->
            (model, Cmd.none)

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    (model, Nav.pushUrl model.navKey (Url.toString url))

                External url ->
                    (model, Nav.load url)

        UrlChanged url ->
            let router = Router.parserUrl url in
            ( { model
              | fretboard = { fretboard | selectedFretPoints = selectFretPoints fretboard.guitar router }
              , route = router
              }
            , Cmd.none
            )

        FretboardMsg fretboardMsg ->
            case fretboardMsg of
                Fretboard.Idle ->
                    (model, Cmd.none)

                Fretboard.SelectTonic note ->
                    let uri = Router.changeValue model.route (note |> Note.toString) in
                    (model, Nav.pushUrl model.navKey uri)


selectFretPoints : Guitar -> Route -> List FretPoint
selectFretPoints guitar route =
    let replaceSpaces = String.replace "-" " " in
    case route of
        RouteRoot ->
            []

        RouteNote val ->
            Url.percentDecode val
                |> Maybe.andThen (Note.fromString)
                |> Maybe.map (singleton >> Guitar.takeNotes guitar)
                |> Maybe.withDefault []

        RouteChord chordKind val ->
            Url.percentDecode val
                |> Maybe.andThen Note.fromString
                |> Maybe.map2 (\chord note -> chord note) (Chord.fromKindString (replaceSpaces chordKind))
                |> Maybe.map (Guitar.takeChord guitar)
                |> Maybe.withDefault []

        RouteScale scaleKind val ->
            Url.percentDecode val
                |> Maybe.andThen Note.fromString
                |> Maybe.map2 (\scale note -> scale note) (Scale.fromKindString (replaceSpaces scaleKind))
                |> Maybe.map (Guitar.takeScale guitar)
                |> Maybe.withDefault []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ Fretboard.view model.fretboard
            |> Html.Styled.toUnstyled
            |> Html.map FretboardMsg
        , div []
            [ div [] [ a [ href "/note/C3" ] [ text "note" ] ]
            , div [] [ a [ href "/chord/major/C3" ] [ text "major chord" ] ]
            , div [] [ a [ href "/chord/minor/C3" ] [ text "minor chord" ] ]
            , div [] [ a [ href "/chord/augmented/C3" ] [ text "augmented chord" ] ]
            , div [] [ a [ href "/chord/diminished/C3" ] [ text "diminished chord" ] ]
            , div [] [ a [ href "/scale/major-pentatonic/C3" ] [ text "major pentatonic scale" ] ]
            , div [] [ a [ href "/scale/minor-pentatonic/C3" ] [ text "minor pentatonic scale" ] ]
            ]
        ]
