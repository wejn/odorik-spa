module Pages.Balance exposing (Model, Msg, init, page, update, view)

import Element exposing (..)
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font
import Element.Background as Background
import Gen.Route as Route
import Html
import Html.Events
import Http
import OdorikApi
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import Task
import Time
import View exposing (View)

page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req shared.storage
        , update = update req shared.storage
        , view = view shared.storage
        , subscriptions = subscriptions
        }

type State
    = Loading
    | NeedLogin
    | Success String
    | Failure String

type alias Model =
    { menu : Element Msg
    , state : State
    , api : OdorikApi.Model
    }

init : Request -> Storage -> ( Model, Cmd Msg )
init req storage =
    case OdorikApi.haveValidCredentials storage.odorikApi of
        True ->
            ( { menu = Shared.menuGen req
              , state = Loading
              , api = storage.odorikApi
              }
            , Cmd.batch [ Task.perform (\_ -> GetBalance) Time.now ]
            )
        False ->
            ( { menu = Shared.menuGen req
              , state = NeedLogin
              , api = storage.odorikApi
              }
            , Cmd.none
            )

type Msg
    = None
    | Login
    | GetBalance
    | GotBalance OdorikApi.ApiResponse


update : Request -> Storage -> Msg -> Model -> ( Model, Cmd Msg )
update req storage msg model =
    case msg of
        None -> ( model , Cmd.none )
        Login -> ( model, Request.pushRoute Route.Settings req )
        GetBalance -> ({ model | state = Loading }, OdorikApi.getBalance model.api GotBalance)
        GotBalance result ->
            case result of
                Ok fullText ->
                    ({ model | state = Success fullText }, Cmd.none)
                Err err ->
                    ({ model | state = Failure (OdorikApi.errorToString err) }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

balanceHelper : Model -> List (Element Msg)
balanceHelper m =
    let
        login = { label = text "Login", onPress = Just Login }
        refresh = { label = text "Refresh", onPress = Just GetBalance }
        (title, explanation, button) =
            case m.state of
                NeedLogin -> ("Not logged in.", "", login)
                Loading -> ("...", "Loading ...", refresh)
                Success b -> (b, "", refresh)
                Failure err -> ("Failed.", "error: " ++ err, refresh)
    in
        [ paragraph
            [Font.size 48, Font.center]
            [ text title ]
        , paragraph
            [Font.size 24, Font.center]
            [ text explanation ]
        , paragraph
            [ Font.center ]
            [ Input.button
                [ padding 10
                , Border.width 2
                , Border.rounded 16
                , Border.color <| rgb255 0x50 0x50 0x50
                , Background.color <| rgb255 0xbb 0xdd 0xff
                , Font.color <| rgb255 255 255 255
                ]
                button
            ]
        ]

view : Storage -> Model -> View Msg
view storage m =
    { title = "Balance"
    , attributes = [width fill, height fill, inFront m.menu]
    , element =
        el [ centerX , centerY, padding 50 ] <|
            column [ spacing 40 ] (balanceHelper m)
    }
