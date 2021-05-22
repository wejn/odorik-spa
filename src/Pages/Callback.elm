module Pages.Callback exposing (Model, Msg, init, page, update, view)

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

type alias Model =
    { menu : Element Msg }

init : Request -> ( Model, Cmd Msg )
init req =
    ( { menu = Shared.menuGen req}, Cmd.none )

type Msg
    = None


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        None -> ( model , Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Storage -> Model -> View Msg
view storage m =
    { title = "Callback"
    , attributes = [width fill, height fill, inFront m.menu]
    , element =
        el [ centerX , centerY, padding 50 ] <|
            column [ spacing 40 ]
            [ paragraph
              []
              [ text "Here be callback." ]
            ]
    }
