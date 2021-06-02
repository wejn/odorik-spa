module OdorikApi exposing
    ( Model
    , ApiResponse
    , init
    , decoder
    , encoder
    , fetchBalance
    , errorToString
    , haveValidCredentials
    , getUser
    , logout
    , verifyCredentials
    , login
    , getCaller
    , getLine
    , fetchLines
    , fetchCallers
    , saveCaller
    , saveLine
    , SpeedDial
    , fetchSpeedDials
    , saveSpeedDials
    , getSpeedDials
    , requestCallback
    )

import Http
import Json.Decode as Json
import Json.Encode as Encode
import Json.Encode.Extra as EE
import Task
import Time
import Url.Builder as UB

type alias Model =
    { user : Maybe String
    , pass : Maybe String
    , line : Maybe String -- technically int, but meh
    , caller : Maybe SpeedDial
    , speedDials : List SpeedDial
    }

type alias ApiResponse a =
    Result Http.Error a

init : Model
init =
    { user = Nothing
    , pass = Nothing
    , line = Nothing
    , caller = Nothing
    , speedDials = []
    }

decoder : Json.Decoder Model
decoder =
    let
        ns = Json.nullable Json.string
    in
    Json.map5 Model
        (Json.field "user" ns)
        (Json.field "pass" ns)
        (Json.field "line" ns)
        (Json.field "caller" <| Json.nullable speedDialDecoder)
        (Json.field "speed_dials" <| Json.list speedDialDecoder)

encoder : Model -> Encode.Value
encoder m =
    Encode.object
        [ ("user", EE.maybe Encode.string m.user)
        , ("pass", EE.maybe Encode.string m.pass)
        , ("line", EE.maybe Encode.string m.line)
        , ("caller", EE.maybe speedDialEncoder m.caller)
        , ("speed_dials", Encode.list speedDialEncoder m.speedDials)
        ]

serverFromCreds : Model -> String
serverFromCreds m =
    case (m.user, m.pass) of
        (Just "888", Just "888") -> "https://wejn.com/odorik_test_api___"
        _ -> "https://www.odorik.cz/api/v1"

apiUrlFromCreds : Model -> String -> List (UB.QueryParameter) -> String
apiUrlFromCreds m api qp =
    UB.custom (UB.CrossOrigin (serverFromCreds m)) [api] (
        [ UB.string "user" (m.user |> Maybe.withDefault "")
        , UB.string "password" (m.pass |> Maybe.withDefault "")
        , UB.string "user_agent" "org.wejn.odorik"
        ] ++ qp) Nothing

errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl _ ->
            "invalid url"
        Http.Timeout ->
            "timeout"
        Http.NetworkError ->
            "network error"
        Http.BadStatus 500 ->
            "server error"
        Http.BadStatus 400 ->
            "call error"
        Http.BadStatus code ->
            "unknown error: " ++ (String.fromInt code)
        Http.BadBody errorMessage ->
            errorMessage

apiResolver : (String -> Result String a) -> Http.Response String -> ApiResponse a
apiResolver =
    \parser ->
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case String.startsWith "error " body of
                        True -> Err (Http.BadBody ("api: " ++ (String.dropLeft 6 (String.trim body))))
                        _ ->
                            case parser body of
                                Err err -> Err (Http.BadBody ("parsing:" ++ err))
                                Ok a -> Ok a

haveValidCredentials : Model -> Bool
haveValidCredentials m =
    case (m.user, m.pass) of
        (Just u, Just p) -> not ((String.isEmpty u) || (String.isEmpty p))
        _ -> False

fetchBalance : Model -> (ApiResponse String -> msg) -> Cmd msg
fetchBalance model msg =
    case haveValidCredentials model of
        False ->
            Task.succeed (msg (Err (Http.BadBody "not logged in"))) |> Task.perform identity
        True ->
            Task.attempt msg (Time.now |> Task.andThen (\time ->
                Http.task
                    { body = Http.emptyBody
                    , timeout = Nothing
                    , headers = []
                    , method = "GET"
                    , url = (apiUrlFromCreds model "balance") [UB.int "t" (Time.posixToMillis time)]
                    , resolver = Http.stringResolver <| apiResolver (\x -> Ok x)
                    } ) )

getUser : Model -> Maybe String
getUser m = m.user

logout : Model -> Model
logout _ = init

login : Model -> String -> String -> Model
login m u p = { m | user = Just u, pass = Just p }

verifyCredentials : (ApiResponse String -> msg) -> String -> String -> Cmd msg
verifyCredentials msg user pass =
    let
        m = { init | user = Just user, pass = Just pass }
    in
        case haveValidCredentials m of
            False -> Task.succeed (msg (Err (Http.BadBody "empty credentials"))) |> Task.perform identity
            True -> fetchBalance m msg

getCaller : Model -> Maybe SpeedDial
getCaller m = m.caller

getLine : Model -> Maybe String
getLine m = m.line

parseLines : String -> Result String (List String)
parseLines data =
    Ok (String.split "," <| String.trim data)

fetchLines : Model -> (ApiResponse (List String) -> msg) -> Cmd msg
fetchLines model msg =
    case haveValidCredentials model of
        False ->
            Task.succeed (msg (Err (Http.BadBody "not logged in"))) |> Task.perform identity
        True ->
            Task.attempt msg (Time.now |> Task.andThen (\time ->
                Http.task
                    { body = Http.emptyBody
                    , timeout = Nothing
                    , headers = []
                    , method = "GET"
                    , url = (apiUrlFromCreds model "lines") [UB.int "t" (Time.posixToMillis time)]
                    , resolver = Http.stringResolver <| apiResolver parseLines
                    } ) )

parseCallers : String -> Result String (List SpeedDial)
parseCallers data =
    Ok (List.map (\n -> { shortcut = 0, name = n, number = n}) <| List.filter (String.startsWith "00") <| String.split "," <| String.trim data)

fetchCallers : Model -> (ApiResponse (List SpeedDial) -> msg) -> Cmd msg
fetchCallers model msg =
    case haveValidCredentials model of
        False ->
            Task.succeed (msg (Err (Http.BadBody "not logged in"))) |> Task.perform identity
        True ->
            Task.attempt msg (Time.now |> Task.andThen (\time ->
                Http.task
                    { body = Http.emptyBody
                    , timeout = Nothing
                    , headers = []
                    , method = "GET"
                    , url = (apiUrlFromCreds model "sms/allowed_sender") [UB.int "t" (Time.posixToMillis time)]
                    , resolver = Http.stringResolver <| apiResolver parseCallers
                    } ) )

saveCaller : Model -> Maybe SpeedDial -> Model
saveCaller m c = { m | caller = c }

saveLine : Model -> Maybe String -> Model
saveLine m l = { m | line = l }

type alias SpeedDial =
    { shortcut : Int
    , number : String
    , name : String
    }

parseSpeedDials : String -> Result String (List SpeedDial)
parseSpeedDials s =
    case Json.decodeString (Json.list speedDialDecoder) s of
        Err e -> Err <| Json.errorToString e
        Ok v -> Ok v

speedDialDecoder : Json.Decoder SpeedDial
speedDialDecoder  =
    Json.map3 SpeedDial
        (Json.field "shortcut" Json.int)
        (Json.field "number" Json.string)
        (Json.field "name" Json.string)

speedDialEncoder : SpeedDial -> Encode.Value
speedDialEncoder m =
    Encode.object
        [ ("shortcut", Encode.int m.shortcut)
        , ("number", Encode.string m.number)
        , ("name", Encode.string m.name)
        ]

fetchSpeedDials : Model -> (ApiResponse (List SpeedDial) -> msg) -> Cmd msg
fetchSpeedDials model msg =
    case haveValidCredentials model of
        False ->
            Task.succeed (msg (Err (Http.BadBody "not logged in"))) |> Task.perform identity
        True ->
            Task.attempt msg (Time.now |> Task.andThen (\time ->
                Http.task
                    { body = Http.emptyBody
                    , timeout = Nothing
                    , headers = []
                    , method = "GET"
                    , url = (apiUrlFromCreds model "speed_dials.json") [UB.int "t" (Time.posixToMillis time)]
                    , resolver = Http.stringResolver <| apiResolver parseSpeedDials
                    } ) )

saveSpeedDials : Model -> List SpeedDial -> Model
saveSpeedDials m c = { m | speedDials = c }

getSpeedDials : Model -> List SpeedDial
getSpeedDials = .speedDials

requestCallback : Model -> String -> String -> Maybe String -> (ApiResponse String -> msg) -> Cmd msg
requestCallback model target caller line msg =
    case haveValidCredentials model of
        False ->
            Task.succeed (msg (Err (Http.BadBody "not logged in"))) |> Task.perform identity
        True ->
            Task.attempt msg (Time.now |> Task.andThen (\time ->
                -- XXX: normally I'd go about building a proper body, but Odorik accepts QS parameters, so meh.
                -- one can use https://github.com/lucamug/elm-form-examples/blob/master/src/Example_2.elm
                Http.task
                    { body = Http.emptyBody
                    , timeout = Nothing
                    , headers = []
                    , method = "POST"
                    , url = (apiUrlFromCreds model "callback") <|
                        ( (Maybe.withDefault [] (Maybe.map (\x -> [UB.string "line" x]) line)) ++
                        [ UB.int "t" (Time.posixToMillis time)
                        , UB.string "recipient" target
                        , UB.string "caller" caller
                        ] )
                    , resolver = Http.stringResolver <| apiResolver (\x -> Ok x)
                    } ) )
