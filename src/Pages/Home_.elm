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
    case req.url.fragment of
        Just str -> case (Base64.decode str) of
            Ok num -> Just num
            Err _ -> Nothing
        Nothing -> Nothing

init : Request -> ( Model, Cmd Msg )
init req =
    ( { number = parseNumber req
      , menu = Shared.menuGen req}
    , Cmd.none )



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

view : Storage -> Model -> View Msg
view storage m =
    { title = "Homepage"
    , attributes = [width fill, height fill, inFront m.menu]
    , element =
        el [ centerX , centerY, padding 50 ] <|
            column [ spacing 40 ]
            [ paragraph
              []
              [ text "Here be homepage." ]
            , paragraph
              []
              [ text (Maybe.withDefault "" m.number) ]
            ]
    }
