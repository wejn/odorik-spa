module Pages.Balance exposing (Model, Msg, init, page, update, view)

import Element exposing (..)
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font
import Element.Background as Background
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
        , update = update shared.storage
        , view = view shared.storage
        , subscriptions = subscriptions
        }

type State
    = Loading
    | Success String
    | Failure String

type alias Model =
    { menu : Element Msg
    , state : State
    , api : OdorikApi.Model
    }

init : Request -> Storage -> ( Model, Cmd Msg )
init req storage =
    ( { menu = Shared.menuGen req
      , state = Loading
      , api = storage.odorikApi
      }, Cmd.batch [ Task.perform (\_ -> GetBalance) Time.now ] )

type Msg
    = None
    | GetBalance
    | GotBalance OdorikApi.ApiResponse


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        None -> ( model , Cmd.none )
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

balanceForModel : Model -> (String, String)
balanceForModel m =
    case m.state of
        Loading -> ("...", "Loading ...")
        Success b -> (b, "")
        Failure err -> ("Failed.", "error: " ++ err)

balanceHelper : Model -> List (Element Msg)
balanceHelper m =
    (balanceForModel m) |> \(title, explanation) ->
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
                { onPress = Just GetBalance
                , label = text "Refresh"
                }
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
