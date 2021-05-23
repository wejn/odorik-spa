module Pages.Balance exposing (Model, Msg, init, page, update, view)

import Element exposing (..)
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font
import Element.Background as Background
import Html
import Html.Events
import Http
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
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
    | Failure

type alias Model =
    { menu : Element Msg
    , state : State
    , balance : String
    , balanceTs : Time.Posix }

init : Request -> Storage -> ( Model, Cmd Msg )
init req storage =
    ( { menu = Shared.menuGen req
      , state = Loading
      , balance = storage.balance
      , balanceTs = storage.balanceTs
      }, loadBalance storage )

type Msg
    = None
    | GotBalance (Result Http.Error String)


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        None -> ( model , Cmd.none )
        GotBalance result ->
            case result of
                Ok fullText ->
                    ({ model | state = (Success fullText), balance = fullText }, Cmd.none) -- FIXME: update refresh TS
                Err _ ->
                    ({ model | state = Failure }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

balanceForModel : Model -> String
balanceForModel m =
    case m.state of
        Loading -> "Loading ..."
        Success _ -> m.balance
        Failure -> "Failed to fetch."

view : Storage -> Model -> View Msg
view storage m =
    { title = "Balance"
    , attributes = [width fill, height fill, inFront m.menu]
    , element =
        el [ centerX , centerY, padding 50 ] <|
            column [ spacing 40 ]
            [ paragraph
              [Font.size 48]
              [ text (balanceForModel m) ]
            ]
    }

loadBalance : Storage -> Cmd Msg
loadBalance storage =
    Http.get
        { url = Shared.apiUrlFromCreds storage "balance" -- FIXME: mod the time, to force refresh
        , expect = Http.expectString GotBalance }
