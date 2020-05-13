port module Main exposing (..)

import Fretboard
import Browser
import Browser.Navigation exposing (Key)
import Guitar
import Html exposing (..)
import Html.Styled
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
init _ _ _ =
    ( FretboardModel
        { guitar =
            { tuning = Guitar.standardTuning
            , fretsNumber = 22
            }
        , selectedFretPoints = []
        }
    , Cmd.none
    )



-- MODEL


type Model
    = FretboardModel Fretboard.Model



-- UPDATE


type Msg
  = Idle
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url
  | FretboardMsg Fretboard.Model Fretboard.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Idle ->
            (model, Cmd.none)

        LinkClicked _ ->
            (model, Cmd.none)

        UrlChanged _ ->
            (model, Cmd.none)

        FretboardMsg fretboardModel fretboardMsg ->
            case fretboardMsg of
                Fretboard.Idle ->
                    (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        fretboard m =
            case m of
                FretboardModel designerModel ->
                    Fretboard.view designerModel
                        |> Html.Styled.toUnstyled
                        |> Html.map (FretboardMsg designerModel)
    in
    div
        []
        [ fretboard model
        ]
