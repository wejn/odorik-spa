module Pages.Home_ exposing (Model, Msg, init, page, update, view)

import Base64
import Dict
import Element exposing (..)
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font
import Element.Background as Background
import Html
import Html.Events
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req
        , update = update shared.storage
        , view = view shared.storage
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { number : Maybe String
    , menu : Element Msg }


parseNumber : Request -> Maybe String
parseNumber req =
    case Dict.get "number" req.query of
        Just str -> case (Base64.decode str) of
            Ok num -> Just num
            Err _ -> Nothing
        Nothing -> Nothing

init : Request -> ( Model, Cmd Msg )
init req =
    ( { number = parseNumber req, menu = Shared.menuGen req}, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        Increment ->
            ( model
            , Storage.increment storage
            )

        Decrement ->
            ( model
            , Storage.decrement storage
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW

buttonStyle =
    [ padding 20
    , Border.width 2
    , Border.rounded 16
    , Border.color <| rgb255 0x50 0x50 0x50
    , Background.color <| rgb255 0xbb 0xdd 0xff
    , Font.color <| rgb255 255 255 255
    , mouseOver
        [ Background.color <| rgb255 255 255 255
        , Font.color <| rgb255 0 0 0 ]
    ]

view : Storage -> Model -> View Msg
view storage m =
    { title = "Homepage"
    , attributes = [width fill, height fill, inFront m.menu]
    , element =
        el [ centerX , centerY, padding 50 ] <|
            column [ spacing 40 ]
            [ paragraph
              []
              [ text ("Number: " ++ (Maybe.withDefault "" m.number)) ]
              -- how the hell do I put this on the next line?
            , row
              [ width fill ]
              [  Input.button
                buttonStyle
                { onPress = Just Decrement
                , label = Element.text "-"
                }
              , el [width fill] <|
                  el [ Font.size 32, centerX ] <|
                    text (" " ++ String.fromInt storage.counter ++ " ")
              , Input.button
                buttonStyle
                { onPress = Just Increment
                , label = Element.text "+"
                }
              ] ]
    }
