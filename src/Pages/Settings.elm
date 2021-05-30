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

type alias Model =
    { menu : Element Msg
    , state : State
    , username : String
    , password : String
    , lastError : String
    , caller : Maybe OdorikApi.SpeedDial
    , callerDropdownState : Dropdown.State OdorikApi.SpeedDial
    , line : Maybe String
    , lineDropdownState : Dropdown.State String
    , callersState : Shared.FetchState
    , callers : List OdorikApi.SpeedDial
    , linesState : Shared.FetchState
    , lines : List String
    , speedDialsState : Shared.FetchState
    , speedDials : List OdorikApi.SpeedDial
    }

fetchAfterLogin : Cmd Msg
fetchAfterLogin =
    Cmd.batch
        [ Task.succeed StartLinesFetch |> Task.perform identity
        , Task.succeed StartCallersFetch |> Task.perform identity
        , Task.succeed StartSpeedDialsFetch |> Task.perform identity
        ]

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
            , callersState = Shared.Fetching
            , callers = []
            , linesState = Shared.Fetching
            , lines = OdorikApi.getLine storage.odorikApi |> Maybe.map (\x -> [x]) |> Maybe.withDefault []
            , speedDialsState = Shared.Fetching
            , speedDials = OdorikApi.getSpeedDials storage.odorikApi
            }
    in
        case OdorikApi.haveValidCredentials storage.odorikApi of
            True -> ( { m | state = LoggedIn } , fetchAfterLogin )
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
    | CallerPicked (Maybe OdorikApi.SpeedDial)
    | LinePicked (Maybe String)
    | CallerDropdownMsg (Dropdown.Msg OdorikApi.SpeedDial)
    | LineDropdownMsg (Dropdown.Msg String)
    | StartLinesFetch
    | LinesFetched (OdorikApi.ApiResponse (List String))
    | StartCallersFetch
    | CallersFetched (OdorikApi.ApiResponse (List OdorikApi.SpeedDial))
    | StartSpeedDialsFetch
    | SpeedDialsFetched (OdorikApi.ApiResponse (List OdorikApi.SpeedDial))


update : Request -> Storage -> Msg -> Model -> ( Model, Cmd Msg )
update req storage msg model =
    case msg of
        None -> ( model , Cmd.none )
        Logout -> ( { model | state = NeedLogin, caller = Nothing, line = Nothing } , Storage.logout storage )
        StartLogin -> ( model , OdorikApi.verifyCredentials VerifyLogin model.username model.password )
        VerifyLogin (Ok _) -> ( model , Storage.login storage FinishLogin model.username model.password )
        VerifyLogin (Err err) -> ( { model | lastError = OdorikApi.errorToString err} , Cmd.none )
        FinishLogin -> ( { model | state = LoggedIn } , fetchAfterLogin )
        ChangeUserName u -> ( { model | username = u } , Cmd.none )
        ChangePassword p -> ( { model | password = p } , Cmd.none )
        UsernameEnter -> ( model, Task.attempt (\_ -> None) (Dom.focus "password") )
        CallerPicked c -> ( { model | caller = c }, Storage.saveCaller storage None c )
        CallerDropdownMsg sm ->
            let
                ( state , cmd ) =
                    Dropdown.update callerConfig sm model model.callerDropdownState
            in
            ( { model | callerDropdownState = state }, cmd )
        LinePicked l -> ( { model | line = l }, Storage.saveLine storage None l )
        LineDropdownMsg sm ->
            let
                ( state , cmd ) =
                    Dropdown.update lineConfig sm model model.lineDropdownState
            in
            ( { model | lineDropdownState = state }, cmd )
        StartLinesFetch ->
            ( model , OdorikApi.fetchLines storage.odorikApi LinesFetched )
        LinesFetched (Err err) ->
            ( { model | linesState = Shared.Error <| OdorikApi.errorToString err } , Cmd.none )
        LinesFetched (Ok a) ->
            case (model.line, a) of
                (Nothing, [first]) ->
                    ( { model | linesState = Shared.Ready, lines = a, line = Just first } , Storage.saveLine storage None (Just first) )
                _ ->
                    ( { model | linesState = Shared.Ready, lines = a } , Cmd.none )
        StartCallersFetch ->
            ( model , OdorikApi.fetchCallers storage.odorikApi CallersFetched )
        CallersFetched (Err err) ->
            ( { model | callersState = Shared.Error <| OdorikApi.errorToString err } , Cmd.none )
        CallersFetched (Ok a) ->
            ( { model | callersState = Shared.Ready, callers = a } , Cmd.none )
        StartSpeedDialsFetch ->
            ( model , OdorikApi.fetchSpeedDials storage.odorikApi SpeedDialsFetched )
        SpeedDialsFetched (Err err) ->
            ( { model | speedDialsState = Shared.Error <| OdorikApi.errorToString err } , Cmd.none )
        SpeedDialsFetched (Ok a) ->
            ( { model | speedDialsState = Shared.Ready, speedDials = a } , Storage.saveSpeedDials storage None a )

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

        itemToElement selected highlighted i =
            let
                bgColor =
                    if selected then
                        rgb255 100 100 100

                    else if highlighted then
                        rgb255 250 250 250

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
        , itemToPrompt = text
        , itemToElement = itemToElement
        }
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSearchAttributes searchAttrs

callerConfig : Dropdown.Config OdorikApi.SpeedDial Msg Model
callerConfig =
    let
        items = (\m -> m.callers ++ m.speedDials)
        selectionFromModel = .caller
        dropdownMsg = CallerDropdownMsg
        itemPickedMsg = CallerPicked
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

        itemToElement selected highlighted item =
            let
                bgColor =
                    if selected then
                        rgb255 100 100 100

                    else if highlighted then
                        rgb255 250 250 250

                    else
                        rgb255 255 255 255
            in
            Shared.speedDialToElement [ Background.color bgColor ] item
    in
    Dropdown.basic
        { itemsFromModel = items
        , selectionFromModel = selectionFromModel
        , dropdownMsg = dropdownMsg
        , onSelectMsg = itemPickedMsg
        , itemToPrompt = text << Shared.speedDialToLabel
        , itemToElement = itemToElement
        }
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSearchAttributes searchAttrs

lineConfig : Dropdown.Config String Msg Model
lineConfig =
    dropdownConfig (\m -> m.lines) .line LineDropdownMsg LinePicked

settingsArea : Model -> List (Element Msg)
settingsArea m =
    case m.state of
        NeedLogin ->
            []
        LoggedIn ->
            [ column [ width fill, spacing 20 ]
                [ column [ width fill, spacing 10 ]
                    -- FIXME: should reflect both speedDialsState and callersState (probably with UI icons)
                    [ row [ width fill ] <| Shared.labelWithSpinner m.speedDialsState "Default caller"
                    , Dropdown.view callerConfig m m.callerDropdownState
                    , link Attr.link
                        { url = "https://www.odorik.cz/ucet/rychle_kontakty.html"
                        , label = el [] <| text "Edit your speed dials."
                        }
                    ]
                , column [ width fill, spacing 10 ]
                    [ row [ width fill ] <| Shared.labelWithSpinner m.linesState "Line"
                    , Dropdown.view lineConfig m m.lineDropdownState
                    ]
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
