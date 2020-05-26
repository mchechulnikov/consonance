port module Main exposing (..)

import Chord exposing (Chord(..))
import Fretboard
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Guitar exposing (FretPoint, Guitar, GuitarFret(..), GuitarString(..))
import Html exposing (..)
import Html.Attributes exposing (href, style)
import Html.Styled
import Interval
import Keyboard
import Keys
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
      , keyboard = { keys = selectKeys route }
      , navKey = key
      , route = route
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { fretboard : Fretboard.Model
    , keyboard : Keyboard.Model
    , navKey : Nav.Key
    , route : Route
    }



-- UPDATE


type Msg
  = Idle
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url
  | FretboardMsg Fretboard.Msg
  | KeyboardMsg Keyboard.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let fretboard = model.fretboard in
    let keyboard = model.keyboard in
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
              , keyboard = { keyboard | keys = selectKeys router }
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

        KeyboardMsg keyboardMsg ->
            case keyboardMsg of
                Keyboard.Idle ->
                    (model, Cmd.none)

                Keyboard.SelectTonic note ->
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

        RouteInterval intervalKind val ->
            Url.percentDecode val
                |> Maybe.andThen Note.fromString
                |> Maybe.map2 (\interval note -> interval note) (Interval.fromKindString (replaceSpaces intervalKind))
                |> Maybe.map (Guitar.takeInterval guitar)
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


selectKeys : Route -> List Keys.Key
selectKeys route =
    let replaceSpaces = String.replace "-" " " in
    case route of
        RouteRoot ->
            Keys.layout

        RouteNote val ->
            Url.percentDecode val
                |> Maybe.andThen (Note.fromString)
                |> Maybe.map (singleton >> Keys.takeNotes)
                |> Maybe.withDefault []

        RouteInterval intervalKind val ->
            Url.percentDecode val
                |> Maybe.andThen Note.fromString
                |> Maybe.map2 (\interval note -> interval note) (Interval.fromKindString (replaceSpaces intervalKind))
                |> Maybe.map Keys.takeInterval
                |> Maybe.withDefault []

        RouteChord chordKind val ->
            Url.percentDecode val
                |> Maybe.andThen Note.fromString
                |> Maybe.map2 (\chord note -> chord note) (Chord.fromKindString (replaceSpaces chordKind))
                |> Maybe.map Keys.takeChord
                |> Maybe.withDefault []

        RouteScale scaleKind val ->
            Url.percentDecode val
                |> Maybe.andThen Note.fromString
                |> Maybe.map2 (\scale note -> scale note) (Scale.fromKindString (replaceSpaces scaleKind))
                |> Maybe.map Keys.takeScale
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
        , Keyboard.view model.keyboard
            |> Html.Styled.toUnstyled
            |> Html.map KeyboardMsg
        , div [ style "display" "flex" ]
            [ div [ style "margin-right" "1rem" ]
                [ div [] [ a [ href "/note/C3" ] [ text "note C3" ] ]
                , div [] [ a [ href "/interval/m2/C3" ] [ text "minor second C3" ] ]
                , div [] [ a [ href "/interval/M2/C3" ] [ text "major second C3" ] ]
                , div [] [ a [ href "/interval/m3/C3" ] [ text "minor third C3" ] ]
                , div [] [ a [ href "/interval/M3/C3" ] [ text "minor second C3" ] ]
                , div [] [ a [ href "/interval/P4/C3" ] [ text "pure fourth C3" ] ]
                , div [] [ a [ href "/interval/A4/C3" ] [ text "triton C3" ] ]
                , div [] [ a [ href "/interval/P5/C3" ] [ text "pure fifth C3" ] ]
                , div [] [ a [ href "/interval/m6/C3" ] [ text "minor sixth C3" ] ]
                , div [] [ a [ href "/interval/M6/C3" ] [ text "major sixth C3" ] ]
                , div [] [ a [ href "/interval/m7/C3" ] [ text "minor seventh C3" ] ]
                , div [] [ a [ href "/interval/M7/C3" ] [ text "major seventh C3" ] ]
                , div [] [ a [ href "/interval/P8/C3" ] [ text "pure octave C3" ] ]
                ]
            , div [ style "margin-right" "1rem" ]
                [ div [] [ a [ href "/chord/major/C3" ] [ text "major chord C3" ] ]
                , div [] [ a [ href "/chord/minor/C3" ] [ text "minor chord C3" ] ]
                , div [] [ a [ href "/chord/augmented/C3" ] [ text "augmented chord C3" ] ]
                , div [] [ a [ href "/chord/diminished/C3" ] [ text "diminished chord C3" ] ]
                , div [] [ a [ href "/chord/fourth/G2" ] [ text "fourth chord G2" ] ]
                , div [] [ a [ href "/chord/fifth/G2" ] [ text "fifth chord G2" ] ]
                , div [] [ a [ href "/chord/major-seventh/G2" ] [ text "major seventh chord G2" ] ]
                , div [] [ a [ href "/chord/minor-seventh/G2" ] [ text "minor seventh chord G2" ] ]
                , div [] [ a [ href "/chord/dominant-seventh/G2" ] [ text "dominant seventh chord G2" ] ]
                , div [] [ a [ href "/chord/diminished-seventh/G2" ] [ text "diminished seventh chord G2" ] ]
                , div [] [ a [ href "/chord/half-diminished-seventh/G2" ] [ text "half-diminished seventh chord G2" ] ]
                , div [] [ a [ href "/chord/minor-major-seventh/G2" ] [ text "minor major seventh chord G2" ] ]
                , div [] [ a [ href "/chord/augmented-major-seventh/G2" ] [ text "augmented major seventh chord G2" ] ]
                , div [] [ a [ href "/chord/dominant-ninth/G2" ] [ text "dominant ninth chord G2" ] ]
                , div [] [ a [ href "/chord/dominant-eleventh/G2" ] [ text "dominant eleventh chord G2" ] ]
                , div [] [ a [ href "/chord/dominant-thirteen/G2" ] [ text "dominant thirteen chord G2" ] ]
                , div [] [ a [ href "/chord/seventh-augmented-fifth/G2" ] [ text "seventh augmented fifth chord G2" ] ]
                , div [] [ a [ href "/chord/seventh-augmented-ninth/G2" ] [ text "seventh augmented ninth chord G2" ] ]
                , div [] [ a [ href "/chord/seventh-sharp-ninth/G2" ] [ text "seventh sharp ninth chord G2" ] ]
                , div [] [ a [ href "/chord/seventh-augmented-eleventh/G2" ] [ text "seventh augmented eleventh chord G2" ] ]
                , div [] [ a [ href "/chord/seventh-diminished-thirteen/G2" ] [ text "seventh diminished thirteen chord G2" ] ]
                , div [] [ a [ href "/chord/add-nine/C3" ] [ text "add nine chord C3" ] ]
                , div [] [ a [ href "/chord/add-fourth/C3" ] [ text "add fourth chord C3" ] ]
                , div [] [ a [ href "/chord/add-six/C3" ] [ text "add six chord C3" ] ]
                , div [] [ a [ href "/chord/six-nine/C3" ] [ text "six nine chord C3" ] ]
                , div [] [ a [ href "/chord/seven-six/C3" ] [ text "seven six chord C3" ] ]
                ]
            , div []
                [ div [] [ a [ href "/scale/major-pentatonic/C3" ] [ text "major pentatonic scale C3" ] ]
                , div [] [ a [ href "/scale/suspended-pentatonic/D3" ] [ text "suspended pentatonic scale D3" ] ]
                , div [] [ a [ href "/scale/blues-minor-pentatonic/E3" ] [ text "blues minor pentatonic scale E3" ] ]
                , div [] [ a [ href "/scale/blues-major-pentatonic/G3" ] [ text "blues major pentatonic scale G3" ] ]
                , div [] [ a [ href "/scale/minor-pentatonic/A3" ] [ text "minor pentatonic scale A3" ] ]
                , div [] [ a [ href "/scale/ionian/C3" ] [ text "ionian scale (natural major) C3" ] ]
                , div [] [ a [ href "/scale/dorian/D3" ] [ text "dorian scale D3" ] ]
                , div [] [ a [ href "/scale/phrygian/E3" ] [ text "phrygian scale E3" ] ]
                , div [] [ a [ href "/scale/lydian/F3" ] [ text "lydian scale F3" ] ]
                , div [] [ a [ href "/scale/myxolydian/G2" ] [ text "myxolydian scale G2" ] ]
                , div [] [ a [ href "/scale/aeolian/A2" ] [ text "aeolian scale (natural minor) A2" ] ]
                , div [] [ a [ href "/scale/locrian/B2" ] [ text "locrian scale B2" ] ]
                , div [] [ a [ href "/scale/chromatic/C3" ] [ text "chromatic scale C3" ] ]
                , div [] [ a [ href "/scale/fourth-circle/G2" ] [ text "fourth scale G2" ] ]
                , div [] [ a [ href "/scale/fifth-circle/G2" ] [ text "fifth scale G2" ] ]
                ]
            ]
        ]
