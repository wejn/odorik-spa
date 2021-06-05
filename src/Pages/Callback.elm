module Pages.Callback exposing (Model, Msg, init, page, update, view, changedUrl)

import Attr
import Delay
import Dropdown
import Element exposing (..)
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font
import Element.Background as Background
import Gen.Route as Route
import Html
import Html.Events
import OdorikApi
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import Task
import Url exposing (Url)
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
    , parseWarning : Maybe String
    , incomingTarget : Maybe OdorikApi.SpeedDial -- incoming via fragment
    , target : Maybe OdorikApi.SpeedDial -- selected in dropbox
    , manualTarget : Maybe OdorikApi.SpeedDial -- synthetic, added by editing text field
    , targetText : String -- text field contents
    , caller : Maybe OdorikApi.SpeedDial -- selected in dropbox
    , manualCaller : Maybe OdorikApi.SpeedDial -- synthetic, added by editing text field
    , callerText : String -- text field contents
    , defaultCaller : Maybe OdorikApi.SpeedDial -- default caller from settings
    , line : Maybe String
    , balance : Maybe String
    , balanceState : Shared.FetchState
    , speedDialsState : Shared.FetchState
    , targetDropdownState : Dropdown.State OdorikApi.SpeedDial
    , callerDropdownState : Dropdown.State OdorikApi.SpeedDial
    , speedDials : List OdorikApi.SpeedDial
    , callbackState : Shared.FetchState
    }

init : Request -> Storage -> ( Model, Cmd Msg )
init req storage =
    let
        (warning, incoming) =
            case Shared.parseFragmentToSpeedDial req.url of
                Err str -> (Just str, Nothing)
                Ok sd -> (Nothing, sd)
        state =
            case OdorikApi.haveValidCredentials storage.odorikApi of
                True -> LoggedIn
                False -> NeedLogin
        caller = OdorikApi.getCaller storage.odorikApi
    in
    (   { state = state
        , parseWarning = warning
        , incomingTarget = incoming
        , target = incoming
        , manualTarget = Nothing
        , targetText = Maybe.withDefault "" (Maybe.map .number incoming)
        , caller = caller
        , manualCaller = Nothing
        , callerText = Maybe.withDefault "" (Maybe.map .number caller)
        , defaultCaller = caller |> Maybe.andThen (\c ->
            case c.shortcut of
                0 -> Just c
                _ -> Nothing )
        , line = OdorikApi.getLine storage.odorikApi
        , balance = Nothing
        , balanceState = Shared.Idle
        , speedDialsState = Shared.Idle
        , targetDropdownState = Dropdown.init "target-dropdown"
        , callerDropdownState = Dropdown.init "caller-dropdown"
        , speedDials = OdorikApi.getSpeedDials storage.odorikApi
        , callbackState = Shared.Idle
        }
    , Cmd.batch
        [ Task.succeed StartBalanceFetch |> Task.perform identity
        , Task.succeed (ChangedUrl req.url) |> Task.perform identity
        ]
    )

type Msg
    = None
    | Login
    | StartBalanceFetch
    | GotBalance (OdorikApi.ApiResponse String)
    | FinishBalanceFetch
    | TargetPicked (Maybe OdorikApi.SpeedDial)
    | CallerPicked (Maybe OdorikApi.SpeedDial)
    | TargetDropdownMsg (Dropdown.Msg OdorikApi.SpeedDial)
    | CallerDropdownMsg (Dropdown.Msg OdorikApi.SpeedDial)
    | StartSpeedDialsFetch
    | SpeedDialsFetched (OdorikApi.ApiResponse (List OdorikApi.SpeedDial))
    | FinishSpeedDialsFetch
    | TargetEdited String
    | CallerEdited String
    | ChangedUrl Url
    | StartCallback
    | GotCallback (OdorikApi.ApiResponse String)
    | FinishCallback

changedUrl : Url -> Msg
changedUrl u = ChangedUrl u

update : Request -> Storage -> Msg -> Model -> ( Model, Cmd Msg )
update req storage msg model =
    case msg of
        None -> ( model , Cmd.none )
        Login -> ( model, Request.pushRoute Route.Settings req )
        StartBalanceFetch -> ( { model | balanceState = Shared.Fetching } , OdorikApi.fetchBalance storage.odorikApi GotBalance )
        GotBalance (Ok b) -> ( { model | balanceState = Shared.Success, balance = Just b } , Delay.after 2000 FinishBalanceFetch)
        GotBalance (Err err) -> ( { model | balanceState = Shared.Error <| OdorikApi.errorToString err, balance = Nothing  } , Cmd.none)
        TargetPicked t -> ( { model | target = t, manualTarget = Nothing, targetText = Maybe.withDefault "" (Maybe.map .number t) }, Cmd.none )
        FinishBalanceFetch ->
            case model.balanceState of
                Shared.Success -> ({ model | balanceState = Shared.Idle }, Cmd.none)
                _ -> ( model , Cmd.none )
        TargetDropdownMsg sm ->
            let
                ( state , cmd ) =
                    Dropdown.update targetConfig sm model model.targetDropdownState
            in
            ( { model | targetDropdownState = state }, cmd )
        CallerPicked c -> ( { model | caller = c, manualCaller = Nothing, callerText = Maybe.withDefault "" (Maybe.map .number c) }, Cmd.none )
        CallerDropdownMsg sm ->
            let
                ( state , cmd ) =
                    Dropdown.update callerConfig sm model model.callerDropdownState
            in
            ( { model | callerDropdownState = state }, cmd )
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
        TargetEdited s ->
            let
                tgt = Just <| Shared.stringToManualSpeedDial s
            in
                ( { model | targetText = s, manualTarget = tgt, target = tgt }, Cmd.none )
        CallerEdited s ->
            let
                tgt = Just <| Shared.stringToManualSpeedDial s
            in
                ( { model | callerText = s, manualCaller = tgt, caller = tgt }, Cmd.none )
        ChangedUrl url ->
            let
                (warning, incoming) =
                    case Shared.parseFragmentToSpeedDial req.url of
                        Err str -> (Just str, Nothing)
                        Ok sd -> (Nothing, sd)
            in
            ( { model
                | parseWarning = warning
                , incomingTarget = incoming
                , target = incoming
                , targetText = Maybe.withDefault "" (Maybe.map .number incoming)
                }
            , Cmd.none )
        StartCallback ->
                        if String.isEmpty model.targetText || String.isEmpty model.callerText then
                            ( { model | callbackState = Shared.Error "need both target and caller filled out" }, Cmd.none )
                        else
                            if model.targetText == model.callerText then
                                ( { model | callbackState = Shared.Error "need different target and caller" }, Cmd.none )
                            else
                                ( { model | callbackState = Shared.Fetching }
                                , OdorikApi.requestCallback storage.odorikApi model.targetText model.callerText model.line GotCallback )
        GotCallback (Err err) ->
            ( { model | callbackState = Shared.Error <| OdorikApi.errorToString err } , Cmd.none )
        GotCallback (Ok a) ->
            ( { model | callbackState = Shared.Success } , Delay.after 5000 FinishCallback )
        FinishCallback ->
            case model.callbackState of
                Shared.Success -> ({ model | callbackState = Shared.Idle }, Cmd.none)
                _ -> ( model , Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

parseWarningIfPresent : Model -> List (Element Msg)
parseWarningIfPresent m =
    case m.parseWarning of
        Nothing -> []
        Just s ->
            [ column [ spacing 10 ]
                [ paragraph Attr.error [ text <| "Error parsing incoming fragment: " ++ s ]
                , paragraph [] [ text "Maybe check if there's a new version of the Shortcuts script?" ]
                ]
            ]

toSpeedDialList : Maybe OdorikApi.SpeedDial -> List OdorikApi.SpeedDial
toSpeedDialList s =
    Maybe.withDefault [] <| Maybe.map (\x -> [x]) s

targetConfig : Dropdown.Config OdorikApi.SpeedDial Msg Model
targetConfig =
    Dropdown.basic
        { itemsFromModel = (\m -> toSpeedDialList m.manualTarget ++ toSpeedDialList m.incomingTarget ++ toSpeedDialList m.defaultCaller ++ m.speedDials)
        , selectionFromModel = .target
        , dropdownMsg = TargetDropdownMsg
        , onSelectMsg = TargetPicked
        , itemToPrompt = text << Shared.speedDialToLabel
        , itemToElement = (\selected highlighted item ->
            Shared.speedDialToElement [ Background.color <| Attr.dropdownBackgroundColor selected highlighted ] item )
        }
        |> Attr.dropdownAugment

callerConfig : Dropdown.Config OdorikApi.SpeedDial Msg Model
callerConfig =
    Dropdown.basic
        { itemsFromModel = (\m -> toSpeedDialList m.manualCaller ++ toSpeedDialList m.incomingTarget ++ toSpeedDialList m.defaultCaller ++ m.speedDials)
        , selectionFromModel = .caller
        , dropdownMsg = CallerDropdownMsg
        , onSelectMsg = CallerPicked
        , itemToPrompt = text << Shared.speedDialToLabel
        , itemToElement = (\selected highlighted item ->
            Shared.speedDialToElement [ Background.color <| Attr.dropdownBackgroundColor selected highlighted ] item )
        }
        |> Attr.dropdownAugment

callbackForm : Model -> List (Element Msg)
callbackForm m =
    [ column [ width fill, spacing 20 ]
        [ column [ width fill, spacing 10 ]
            [ Input.text []
                { onChange = TargetEdited
                , text = m.targetText
                , placeholder = Just <| Input.placeholder [] <| text "Dialled second"
                , label = Input.labelAbove [ alignLeft ] <| text "Target"
                }
            , Dropdown.view targetConfig m m.targetDropdownState
            ]
        , column [ width fill, spacing 10 ]
            [ Input.text []
                { onChange = CallerEdited
                , text = m.callerText
                , placeholder = Just <| Input.placeholder [] <| text "Dialled first"
                , label = Input.labelAbove [ alignLeft ] <| text "Caller"
                }
            , Dropdown.view callerConfig m m.callerDropdownState
            ]
        , column [ width fill, spacing 10 ]
            [ text "Line"
            , Maybe.withDefault (text "none") (Maybe.map (text) m.line)
            ]
        , row [ width fill, Font.center ]
            [ Input.button
                (width fill :: Attr.button)
                { label = text "Callback"
                , onPress = Just StartCallback
                }
            ]
        , callbackStateHelper m.callbackState
        , row [ width fill ] <| Shared.labelWithSpinner m.balanceState ("Balance: " ++ Maybe.withDefault "??" m.balance) (Just StartBalanceFetch)
        , row [ width fill ] <| Shared.labelWithSpinner m.speedDialsState ("SpeedDials...") (Just StartSpeedDialsFetch)
        , row [ paddingXY 0 20, width fill ] [ text "" ] -- make space at the bottom
        ]
    ]

callbackStateHelper : Shared.FetchState -> Element Msg
callbackStateHelper c =
    case c of
        Shared.Idle ->
            row [ width fill ] [ text "" ]
        Shared.Fetching ->
            row [ width fill ]
                [ paragraph [ Font.alignLeft ] [ text "Requesting callback..." ]
                , paragraph [ width shrink, Font.alignRight ] [ Attr.spinnerAnimatedIcon 20 20 ]
                ]
        Shared.Success ->
            row ( width fill :: Attr.success )
                [ paragraph [ Font.alignLeft] [ text "Successfully requested." ]
                , paragraph [ width shrink, Font.alignRight ] [ Attr.checkmarkIcon 20 20 ]
                ]
        Shared.Error err ->
            row ( width fill :: Attr.error )
                [ paragraph [ Font.alignLeft] [ text <| "Failed: " ++ err ]
                , paragraph [ width shrink, Font.alignRight ] [ Attr.crossIcon 20 20 ]
                ]

view : Request -> Shared.Model -> Model -> View Msg
view req shared m =
    Shared.view shared req "Callback" <|
        column [ width fill, height fill, spacing 40 ]
            (  parseWarningIfPresent m
            ++ case m.state of
                NeedLogin -> Shared.loginForm Login
                LoggedIn -> callbackForm m
            )
