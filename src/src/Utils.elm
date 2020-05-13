module Utils exposing (..)


import Html.Styled exposing (Attribute)
import Html.Styled.Events exposing (on, preventDefaultOn, stopPropagationOn)
import Json.Decode as Decode



updateIfTrue : (a -> a) -> a -> Bool -> a
updateIfTrue fn a shouldBeDone =
    if shouldBeDone then
        fn a

    else
        a



-- BOOL


boolToMaybe : a -> Bool -> Maybe a
boolToMaybe a bool =
    if bool then
        Just a

    else
        Nothing


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"



-- HTML EVENTS


onStd : String -> msg -> Attribute msg
onStd eventName msg =
    on eventName (Decode.succeed msg)

onWithoutProp : String -> msg -> Attribute msg
onWithoutProp eventName msg =
    stopPropagationOn eventName (Decode.succeed (msg, True))

onWithoutDef : String -> msg -> Attribute msg
onWithoutDef eventName msg =
    preventDefaultOn eventName (Decode.succeed (msg, True))