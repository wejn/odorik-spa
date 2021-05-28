module Pages.Settings exposing (Model, Msg, init, page, update, view)

import Attr
import Browser.Dom as Dom
import Dropdown
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

type FetchState
    = Fetching
    | Ready

type alias Model =
    { menu : Element Msg
    , state : State
    , username : String
    , password : String
    , lastError : String
    , caller : Maybe String
    , callerDropdownState : Dropdown.State String
    , line : Maybe String
    , lineDropdownState : Dropdown.State String
    , callersState : FetchState
    , callers : List String
    , linesState : FetchState
    , lines : List String
    }

init : Request -> Storage -> ( Model, Cmd Msg )
init req storage =
    let
        m = { menu = Shared.menuGen req
            , state = NeedLogin
            , username = ""
            , password = ""
            , lastError = ""
            , caller = OdorikApi.getCaller storage.odorikApi
            , callerDropdownState = Dropdown.init "caller-dropdown"
            , line = OdorikApi.getLine storage.odorikApi
            , lineDropdownState = Dropdown.init "line-dropdown"
            , callersState = Fetching
            , callers = OdorikApi.getCaller storage.odorikApi |> Maybe.map (\x -> [x]) |> Maybe.withDefault []
            , linesState = Fetching
            , lines = OdorikApi.getLine storage.odorikApi |> Maybe.map (\x -> [x]) |> Maybe.withDefault []
            }
    in
        case OdorikApi.haveValidCredentials storage.odorikApi of
            True -> ( { m | state = LoggedIn } , Cmd.none )
            False -> ( { m | state = NeedLogin } , Cmd.none )

type Msg
    = None
    | Logout
    | StartLogin
    | VerifyLogin (OdorikApi.ApiResponse String)
    | FinishLogin
    | ChangePassword String
    | ChangeUserName String
    | UsernameEnter
    | CallerPicked (Maybe String)
    | LinePicked (Maybe String)
    | CallerDropdownMsg (Dropdown.Msg String)
    | LineDropdownMsg (Dropdown.Msg String)


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
        CallerPicked c -> ( { model | caller = c }, Cmd.none )
        CallerDropdownMsg sm ->
            let
                ( state , cmd ) =
                    Dropdown.update callerConfig sm model model.callerDropdownState
            in
            ( { model | callerDropdownState = state }, cmd )
        LinePicked l -> ( { model | line = l }, Cmd.none )
        LineDropdownMsg sm ->
            let
                ( state , cmd ) =
                    Dropdown.update lineConfig sm model model.lineDropdownState
            in
            ( { model | lineDropdownState = state }, cmd )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

loginPage : Model -> Element Msg
loginPage model =
    column [ paddingXY 0 20, spacingXY 0 10, width (px 300), centerX ]
        [ paragraph [ Font.bold, Font.size 24, Font.alignLeft ] [ text "Login" ]
        , Input.username ( Shared.onEnter UsernameEnter :: Attr.input)
            { onChange = ChangeUserName
            , text = model.username
            , label = Input.labelAbove [ Font.alignLeft ] <| text "User name:"
            , placeholder = Nothing
            }
        , Input.currentPassword ( [htmlAttribute <| Html.Attributes.id "password", Shared.onEnter StartLogin] ++ Attr.input)
            { onChange = ChangePassword
            , text = model.password
            , label = Input.labelAbove [ Font.alignLeft ] <| text "Password:"
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
        LoggedIn ->
            [ row [ spacing 10 ]
                [ paragraph
                    [ width <| fillPortion 2, Font.center ]
                    [ text ("Logged in as: " ++ (OdorikApi.getUser storage.odorikApi |> Maybe.withDefault "???")) ]
                , paragraph
                    [ width <| fillPortion 1, Font.center ]
                    [ Input.button
                        Attr.button
                        { label = text "Logout"
                        , onPress = Just Logout }
                    ]
                ]
            ]

dropdownConfig : (Model -> List String) -> (Model -> Maybe String) -> (Dropdown.Msg String -> Msg) -> (Maybe String -> Msg) -> Dropdown.Config String Msg Model
dropdownConfig items selectionFromModel dropdownMsg itemPickedMsg =
    let
        containerAttrs =
            [ width (px 300) ]

        selectAttrs =
            [ Border.width 1, Border.rounded 5, paddingXY 16 8, spacing 10, width fill ]

        searchAttrs =
            [ Border.width 0, padding 0 ]

        listAttrs =
            [ Border.width 1
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
            , width fill
            , spacing 0
            ]

        itemToPrompt item =
            text item

        itemToElement selected highlighted i =
            let
                bgColor =
                    if highlighted then
                        rgb255 250 250 250

                    else if selected then
                        rgb255 100 100 100

                    else
                        rgb255 255 255 255
            in
            el
                [ Background.color bgColor
                , padding 8
                , spacing 10
                , width fill
                ]
                (text i)
    in
    Dropdown.basic
        { itemsFromModel = items
        , selectionFromModel = selectionFromModel
        , dropdownMsg = dropdownMsg
        , onSelectMsg = itemPickedMsg
        , itemToPrompt = itemToPrompt
        , itemToElement = itemToElement
        }
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSearchAttributes searchAttrs

callerConfig : Dropdown.Config String Msg Model
callerConfig =
    dropdownConfig (\m -> m.callers) .caller CallerDropdownMsg CallerPicked

lineConfig : Dropdown.Config String Msg Model
lineConfig =
    dropdownConfig (\m -> m.lines) .line LineDropdownMsg LinePicked

-- labelWithSpinner : FetchState -> String -> ?
labelWithSpinner s t =
    case s of
        Fetching ->
            [ paragraph [ Font.alignLeft ] [ text t ]
            , paragraph [ Font.alignRight ] [ text "(loading...)" ]
            ]
        Ready ->
            [ paragraph [ Font.alignLeft ] [ text t ]
            ]


settingsArea : Model -> List (Element Msg)
settingsArea m =
    case m.state of
        NeedLogin ->
            []
        LoggedIn ->
            [ column [ width fill, spacing 20 ]
                [ column [ width fill, spacing 10 ]
                    [ row [ width fill ] <| labelWithSpinner m.callersState "Default caller"
                    , Dropdown.view callerConfig m m.callerDropdownState
                    ]
                , column [ width fill, spacing 10 ]
                    [ row [ width fill ] <| labelWithSpinner m.linesState "Line"
                    , Dropdown.view lineConfig m m.lineDropdownState
                    ]
                ]
                , row [ width fill ]
                    [ Input.button
                        Attr.button
                        { label = text "Save"
                        , onPress = Just None } -- FIXME
                    ]
            ]

view : Storage -> Model -> View Msg
view storage m =
    { title = "Settings"
    , attributes = [width fill, height fill, inFront m.menu]
    , element =
        el [ centerX , centerY, padding 50 ] <|
            column [ spacing 40 ] ( loginArea storage m ++ settingsArea m)
    }
