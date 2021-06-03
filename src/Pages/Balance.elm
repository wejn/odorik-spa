module Pages.Balance exposing (Model, Msg, init, page, update, view)

import Attr
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
import View exposing (View)

page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req shared.storage
        , update = update req shared.storage
        , view = view req shared
        , subscriptions = subscriptions
        }

type State
    = NeedLogin
    | LoggedIn

type alias Model =
    { state : State
    , balance : String
    , balanceState : Shared.FetchState
    }

init : Request -> Storage -> ( Model, Cmd Msg )
init req storage =
    case OdorikApi.haveValidCredentials storage.odorikApi of
        True ->
            update req storage (StartBalanceFetch) <|
                { balance = "???.??", state = LoggedIn, balanceState = Shared.Fetching }
        False ->
            ( { balance = "???.??", state = NeedLogin, balanceState = Shared.Fetching } , Cmd.none )

type Msg
    = None
    | Login
    | StartBalanceFetch
    | GotBalance (OdorikApi.ApiResponse String)


update : Request -> Storage -> Msg -> Model -> ( Model, Cmd Msg )
update req storage msg model =
    case msg of
        None -> ( model , Cmd.none )
        Login -> ( model, Request.pushRoute Route.Settings req )
        StartBalanceFetch -> ({ model | state = LoggedIn, balanceState = Shared.Fetching }, OdorikApi.fetchBalance storage.odorikApi GotBalance)
        GotBalance (Ok fullText) -> ({ model | balanceState = Shared.Ready, balance = fullText }, Cmd.none)
        GotBalance (Err err) -> ({ model | balanceState = Shared.Error (OdorikApi.errorToString err) }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

balanceHelper : Model -> List (Element Msg)
balanceHelper m =
    let
        button = (\x -> Input.button Attr.linkLikeButton { onPress = Just StartBalanceFetch, label = x } )
        ( icon, attr, explanation ) =
            case m.balanceState of
                Shared.Fetching -> ( Attr.spinnerAnimatedIcon, [], " " )
                Shared.Ready -> ( Attr.spinnerIcon, [], " " )
                Shared.Error err -> ( Attr.crossIcon, Attr.error, err )
    in
    [ paragraph
        [Font.size 48, Font.center]
        [ text m.balance ]
    , paragraph (Font.center :: attr ) [ button <| icon 40 40 ]
    , paragraph (Font.center :: attr ) [ text explanation ]
    ]

view : Request -> Shared.Model -> Model -> View Msg
view req shared m =
    Shared.view shared req "Balance" <|
        column [ width fill, height fill, spacing 40 ] <|
            case m.state of
                NeedLogin -> Shared.loginForm Login
                LoggedIn -> balanceHelper m
