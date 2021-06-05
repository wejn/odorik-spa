module Pages.Settings exposing (Model, Msg, init, page, update, view)

import Attr
import Browser.Dom as Dom
import Delay
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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
        , view = view req shared
        , subscriptions = subscriptions
        }

type State
    = LoggedIn
    | NeedLogin

type alias Model =
    { state : State
    , username : String
    , password : String
    , lastError : String
    , caller : Maybe OdorikApi.SpeedDial -- selected in dropbox
    , callerDropdownState : Dropdown.State OdorikApi.SpeedDial
    , callerText : String -- text field contents
    , manualCaller : Maybe OdorikApi.SpeedDial -- synthetic, added by editing text field
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
        caller = OdorikApi.getCaller storage.odorikApi
        m = { state = NeedLogin
            , username = ""
            , password = ""
            , lastError = ""
            , caller = caller
            , callerText = Maybe.withDefault "" (Maybe.map .number caller)
            , manualCaller = caller |> Maybe.andThen (\c ->
                case c.shortcut of
                    0 -> Just c
                    _ -> Nothing ) |> Maybe.andThen (\c ->
                        if c.number == c.name then
                            Nothing
                        else
                            Just c )
            , callerDropdownState = Dropdown.init "caller-dropdown"
            , line = OdorikApi.getLine storage.odorikApi
            , lineDropdownState = Dropdown.init "line-dropdown"
            , callersState = Shared.Idle
            , callers = []
            , linesState = Shared.Idle
            , lines = OdorikApi.getLine storage.odorikApi |> Maybe.map (\x -> [x]) |> Maybe.withDefault []
            , speedDialsState = Shared.Idle
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
    | FinishLinesFetch
    | StartCallersFetch
    | CallersFetched (OdorikApi.ApiResponse (List OdorikApi.SpeedDial))
    | FinishCallersFetch
    | StartSpeedDialsFetch
    | SpeedDialsFetched (OdorikApi.ApiResponse (List OdorikApi.SpeedDial))
    | FinishSpeedDialsFetch
    | CallerEdited String


update : Request -> Storage -> Msg -> Model -> ( Model, Cmd Msg )
update req storage msg model =
    case msg of
        None -> ( model , Cmd.none )
        Logout -> ( { model | state = NeedLogin, caller = Nothing, line = Nothing, manualCaller = Nothing, callerText = "" } , Storage.logout storage )
        StartLogin -> ( model , OdorikApi.verifyCredentials VerifyLogin model.username model.password )
        VerifyLogin (Ok _) -> ( model , Storage.login storage FinishLogin model.username model.password )
        VerifyLogin (Err err) -> ( { model | lastError = OdorikApi.errorToString err} , Cmd.none )
        FinishLogin -> ( { model | state = LoggedIn } , fetchAfterLogin )
        ChangeUserName u -> ( { model | username = u } , Cmd.none )
        ChangePassword p -> ( { model | password = p } , Cmd.none )
        UsernameEnter -> ( model, Task.attempt (\_ -> None) (Dom.focus "password") )
        CallerPicked c ->
            ( { model | caller = c, manualCaller = Nothing, callerText = Maybe.withDefault "" (Maybe.map .number c) }
            , Storage.saveCaller storage None c )
        CallerDropdownMsg sm ->
            let
                ( state , cmd ) =
                    Dropdown.update callerConfig sm model model.callerDropdownState
            in
            ( { model | callerDropdownState = state }, cmd )
        CallerEdited s ->
            let
                caller = Just <| Shared.stringToManualSpeedDial s
            in
            ( { model | callerText = s, manualCaller = caller, caller = caller }, Storage.saveCaller storage None caller )
        LinePicked l -> ( { model | line = l }, Storage.saveLine storage None l )
        LineDropdownMsg sm ->
            let
                ( state , cmd ) =
                    Dropdown.update lineConfig sm model model.lineDropdownState
            in
            ( { model | lineDropdownState = state }, cmd )
        StartLinesFetch ->
            ( { model | linesState = Shared.Fetching } , OdorikApi.fetchLines storage.odorikApi LinesFetched )
        LinesFetched (Err err) ->
            ( { model | linesState = Shared.Error <| OdorikApi.errorToString err } , Cmd.none )
        LinesFetched (Ok a) ->
            let
                newModel = { model | linesState = Shared.Success, lines = a }
                finish = Delay.after 2000 FinishLinesFetch
            in
            case (model.line, a) of
                (Nothing, [first]) ->
                    ( { newModel | line = Just first }
                    , Cmd.batch
                        [ Storage.saveLine storage None (Just first)
                        , finish
                        ]
                    )
                _ ->
                    ( newModel , finish )
        FinishLinesFetch ->
            case model.linesState of
                Shared.Success -> ({ model | linesState = Shared.Idle }, Cmd.none)
                _ -> ( model , Cmd.none )
        StartCallersFetch ->
            ( { model | callersState = Shared.Fetching } , OdorikApi.fetchCallers storage.odorikApi CallersFetched )
        CallersFetched (Err err) ->
            ( { model | callersState = Shared.Error <| OdorikApi.errorToString err } , Cmd.none )
        CallersFetched (Ok a) ->
            ( { model | callersState = Shared.Success, callers = a } , Delay.after 2000 FinishCallersFetch )
        FinishCallersFetch ->
            case model.callersState of
                Shared.Success -> ({ model | callersState = Shared.Idle }, Cmd.none)
                _ -> ( model , Cmd.none )
        StartSpeedDialsFetch ->
            ( { model | speedDialsState = Shared.Fetching } , OdorikApi.fetchSpeedDials storage.odorikApi SpeedDialsFetched )
        SpeedDialsFetched (Err err) ->
            ( { model | speedDialsState = Shared.Error <| OdorikApi.errorToString err } , Cmd.none )
        SpeedDialsFetched (Ok a) ->
            ( { model | speedDialsState = Shared.Success, speedDials = a }
            , Cmd.batch
                [ Storage.saveSpeedDials storage None a
                , Delay.after 2000 FinishSpeedDialsFetch
                ]
            )
        FinishSpeedDialsFetch ->
            case model.speedDialsState of
                Shared.Success -> ({ model | speedDialsState = Shared.Idle }, Cmd.none)
                _ -> ( model , Cmd.none )

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
            [ row [ spacing 10, width fill ]
                [ paragraph
                    [ width <| fillPortion 2, Font.alignLeft ]
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

callerConfig : Dropdown.Config OdorikApi.SpeedDial Msg Model
callerConfig =
    Dropdown.basic
        { itemsFromModel = (\m -> (Maybe.withDefault [] <| Maybe.map (\x -> [x]) m.manualCaller) ++ m.callers ++ m.speedDials)
        , selectionFromModel = .caller
        , dropdownMsg = CallerDropdownMsg
        , onSelectMsg = CallerPicked
        , itemToPrompt = text << Shared.speedDialToLabel
        , itemToElement = (\selected highlighted item ->
            Shared.speedDialToElement [ Background.color <| Attr.dropdownBackgroundColor selected highlighted ] item )
        }
        |> Attr.dropdownAugment

lineConfig : Dropdown.Config String Msg Model
lineConfig =
    Dropdown.basic
        { itemsFromModel = (\m -> m.lines)
        , selectionFromModel = .line
        , dropdownMsg = LineDropdownMsg
        , onSelectMsg = LinePicked
        , itemToPrompt = text
        , itemToElement = (\selected highlighted item ->
            el
                [ Background.color <| Attr.dropdownBackgroundColor selected highlighted
                , padding 8
                , spacing 10
                , width fill
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 200 200 200
                ]
                (text item) )

        }
        |> Attr.dropdownAugment

settingsArea : Model -> List (Element Msg)
settingsArea m =
    case m.state of
        NeedLogin ->
            []
        LoggedIn ->
            [ column [ width fill, spacing 20 ]
                [ column [ width fill, spacing 10 ]
                    -- FIXME: should reflect both speedDialsState and callersState (probably with UI icons)
                    [ row [ width fill ] <| Shared.labelWithSpinner m.speedDialsState "Default caller" (Just StartSpeedDialsFetch)
                    , column [ width fill, spacing 10 ]
                        [ Input.text []
                            { onChange = CallerEdited
                            , text = m.callerText
                            , placeholder = Just <| Input.placeholder [] <| text "Used on callback page"
                            , label = Input.labelHidden "Default caller" -- NOT using the spinner here because clicking on "Refresh" opens kbd on ios
                            }
                        ]
                    , Dropdown.view callerConfig m m.callerDropdownState
                    , newTabLink Attr.link
                        { url = "https://www.odorik.cz/ucet/rychle_kontakty.html"
                        , label = el [] <| text "Edit your speed dials."
                        }
                    ]
                , column [ width fill, spacing 10 ]
                    [ row [ width fill ] <| Shared.labelWithSpinner m.linesState "Line" (Just StartLinesFetch)
                    , Dropdown.view lineConfig m m.lineDropdownState
                    ]
                ]
            ]

view : Request -> Shared.Model -> Model -> View Msg
view req shared m =
    Shared.view shared req "Settings" <|
        column [ width fill, height fill, spacing 40 ] ( loginArea shared.storage m ++ settingsArea m )
