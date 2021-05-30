module Pages.Callback exposing (Model, Msg, init, page, update, view)

import Attr
import Element exposing (..)
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font
import Element.Background as Background
import Html
import Html.Events
import OdorikApi
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import View exposing (View)

page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req shared.storage
        , update = update shared.storage
        , view = view shared.storage
        , subscriptions = subscriptions
        }

type alias Model =
    { menu : Element Msg
    , parseWarning : Maybe String
    , target : Maybe OdorikApi.SpeedDial
    , caller : Maybe OdorikApi.SpeedDial
    , line : Maybe String
    }

init : Request -> Storage -> ( Model, Cmd Msg )
init req storage =
    let
        (warning, target) =
            case Shared.parseFragmentToSpeedDial req.url of
                Err str -> (Just str, Nothing)
                Ok sd -> (Nothing, sd)
    in
    (   { menu = Shared.menuGen req
        , parseWarning = warning
        , target = target
        , caller = OdorikApi.getCaller storage.odorikApi
        , line = OdorikApi.getLine storage.odorikApi
        }
    , Cmd.none
    )

type Msg
    = None


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        None -> ( model , Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

parseWarningIfPresent : Model -> List (Element Msg)
parseWarningIfPresent m =
    case m.parseWarning of
        Nothing -> []
        Just s ->
            [ column [ spacing 10 ]
                [ paragraph Attr.error [ text <| "Error parsing incoming fragment: " ++ s ]
                , paragraph [] [ text "Maybe check if there's a new version of the Shortcuts script?" ]
                ]
            ]

callbackForm : Model -> List (Element Msg)
callbackForm m =
    [ column [ width fill, spacing 20 ]
        [ column [ width fill, spacing 10 ]
            [ text "Target"
            , Maybe.withDefault (text "none") (Maybe.map (Shared.speedDialToElement []) m.target)
            ]
        , column [ width fill, spacing 10 ]
            [ text "Caller"
            , Maybe.withDefault (text "none") (Maybe.map (Shared.speedDialToElement []) m.caller)
            ]
        , column [ width fill, spacing 10 ]
            [ text "Line"
            , Maybe.withDefault (text "none") (Maybe.map (text) m.line)
            ]
        , row [ width fill, Font.center ]
            [ Input.button
                (width fill :: Attr.button)
                { label = text "Callback"
                , onPress = Just None -- FIXME
                }
            ]
        , row [ width fill ]
            [ text "Balance: ??" ]
        ]
    ]

view : Storage -> Model -> View Msg
view storage m =
    { title = "Callback"
    , attributes = [width fill, height fill, inFront m.menu]
    , element =
        el [ centerX , centerY, padding 50 ] <|
            column [ spacing 40 ]
                (  parseWarningIfPresent m
                ++ callbackForm m
                )
    }
