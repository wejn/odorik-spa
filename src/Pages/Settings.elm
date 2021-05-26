module Pages.Settings exposing (Model, Msg, init, page, update, view)

import Attr
import Browser.Dom as Dom
import Element exposing (..)
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font
import Element.Background as Background
import Html
import Html.Events
import Html.Attributes
import OdorikApi
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import Task
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
    = LoggedIn
    | NeedLogin

type alias Model =
    { menu : Element Msg
    , state : State
    , username : String
    , password : String
    , lastError : String
    }

init : Request -> Storage -> ( Model, Cmd Msg )
init req storage =
    let
        m = { menu = Shared.menuGen req
            , state = NeedLogin
            , username = ""
            , password = ""
            , lastError = ""
            }
    in
        case OdorikApi.haveValidCredentials storage.odorikApi of
            True -> ( { m | state = LoggedIn } , Cmd.none )
            False -> ( { m | state = NeedLogin } , Cmd.none )

type Msg
    = None
    | Logout
    | StartLogin
    | VerifyLogin OdorikApi.ApiResponse
    | FinishLogin
    | ChangePassword String
    | ChangeUserName String
    | UsernameEnter


update : Request -> Storage -> Msg -> Model -> ( Model, Cmd Msg )
update req storage msg model =
    case msg of
        None -> ( model , Cmd.none )
        Logout -> ( { model | state = NeedLogin } , Storage.logout storage )
        StartLogin -> ( model , OdorikApi.verifyCredentials VerifyLogin model.username model.password )
        VerifyLogin (Ok _) -> ( model , Storage.login storage FinishLogin model.username model.password )
        VerifyLogin (Err err) -> ( { model | lastError = OdorikApi.errorToString err} , Cmd.none )
        FinishLogin -> ( { model | state = LoggedIn } , Cmd.none )
        ChangeUserName u -> ( { model | username = u } , Cmd.none )
        ChangePassword p -> ( { model | password = p } , Cmd.none )
        UsernameEnter -> ( model, Task.attempt (\_ -> None) (Dom.focus "password") )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

loginPage : Model -> Element Msg
loginPage model =
    column [ paddingXY 0 20, spacingXY 0 10, width (px 300), centerX ]
        [ Input.username ( Shared.onEnter UsernameEnter :: Attr.input)
            { onChange = ChangeUserName
            , text = model.username
            , label = Input.labelAbove [] <| text "User name:"
            , placeholder = Nothing
            }
        , Input.currentPassword ( [htmlAttribute <| Html.Attributes.id "password", Shared.onEnter StartLogin] ++ Attr.input)
            { onChange = ChangePassword
            , text = model.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.button Attr.button
            { onPress = Just StartLogin
            , label = el [ centerX ] <| text "Login"
            }
            , el Attr.error <| text model.lastError
        ]

loginArea : Storage -> Model -> List (Element Msg)
loginArea storage m =
    case m.state of
        NeedLogin ->
            [ paragraph
                [ Font.center ]
                [ loginPage m ]
            ]
        _ ->
            [ paragraph
                [Font.size 24, Font.center ]
                [ text ("Logged in as: " ++ OdorikApi.getUser storage.odorikApi) ]
            , paragraph
                [ Font.center ]
                [ Input.button
                    Attr.button
                    { label = text "Logout"
                    , onPress = Just Logout }
                ]
            ]

settingsArea : Model -> List (Element Msg)
settingsArea m =
    case m.state of
        NeedLogin ->
            []
        _ ->
            [ paragraph
                [ Font.center ]
                [ text "(the rest)" ] -- FIXME: Implement the rest of settings (caller, line, ...)
            ]

view : Storage -> Model -> View Msg
view storage m =
    { title = "Settings"
    , attributes = [width fill, height fill, inFront m.menu]
    , element =
        el [ centerX , centerY, padding 50 ] <|
            column [ spacing 40 ] ( loginArea storage m ++ settingsArea m)
    }
