module Pages.Home_ exposing (Model, Msg, init, page, update, view)

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
        , view = view req shared.storage
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {
    }


init : Request -> ( Model, Cmd Msg )
init req =
    ( { } , Cmd.none )



-- UPDATE


type Msg
    = None


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        None -> ( model , Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW

view : Request -> Storage -> Model -> View Msg
view req storage m =
    Shared.view req "Homepage" <|
        column [ spacing 40 ]
        [ paragraph
          []
          [ text "Here be homepage." ]
        ]
