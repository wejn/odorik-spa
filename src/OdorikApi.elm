module OdorikApi exposing
    ( Model
    , ApiResponse
    , init
    , decoder
    , encoder
    , getBalance
    , errorToString
    , haveValidCredentials
    , getUser
    , logout
    , verifyCredentials
    , login
    , getCaller
    , getLine
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
    , caller : Maybe String
    }

type alias ApiResponse =
    Result Http.Error String

init : Model
init =
    { user = Nothing
    , pass = Nothing
    , line = Nothing
    , caller = Nothing
    }

decoder : Json.Decoder Model
decoder =
    let
        ns = Json.nullable Json.string
    in
    Json.map4 Model
        (Json.field "user" ns)
        (Json.field "pass" ns)
        (Json.field "line" ns)
        (Json.field "caller" ns)

encoder : Model -> Encode.Value
encoder m =
    Encode.object
        [ ("user", EE.maybe Encode.string m.user)
        , ("pass", EE.maybe Encode.string m.pass)
        , ("line", EE.maybe Encode.string m.line)
        , ("caller", EE.maybe Encode.string m.caller)
        ]

serverFromCreds : Model -> String
serverFromCreds m =
    case (m.user, m.pass) of
        (Just "888", Just "888") -> "https://wejn.com/odorik_test_api___"
        _ -> "https://www.odorik.cz/api/v1"

apiUrlFromCreds : Model -> String -> List (UB.QueryParameter) -> String
apiUrlFromCreds m api qp =
    UB.custom (UB.CrossOrigin (serverFromCreds m)) [api] ([UB.string "user" (m.user |> Maybe.withDefault ""), UB.string "password" (m.pass |> Maybe.withDefault "")] ++ qp) Nothing

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

expectApiResponse : (ApiResponse -> msg) -> Http.Expect msg
expectApiResponse toMsg =
    Http.expectStringResponse toMsg apiResolver

apiResolver : Http.Response String -> ApiResponse
apiResolver =
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
                        _ -> Ok body

haveValidCredentials : Model -> Bool
haveValidCredentials m =
    case (m.user, m.pass) of
        (Just u, Just p) -> not ((String.isEmpty u) || (String.isEmpty p))
        _ -> False

getBalance : Model -> (ApiResponse -> msg) -> Cmd msg
getBalance model msg =
    case haveValidCredentials model of
        False ->
            Task.succeed (msg (Err (Http.BadBody "not logged in"))) |> Task.perform identity
        True ->
            {-
            Http.get
               { url = apiUrlFromCreds model "balance" [] -- FIXME: mod the time, to force refresh
               , expect = expectApiResponse msg }
            -}
            Task.attempt msg (Time.now |> Task.andThen (\time ->
                Http.task
                    { body = Http.emptyBody
                    , timeout = Nothing
                    , headers = []
                    , method = "GET"
                    , url = (apiUrlFromCreds model "balance") [UB.int "t" (Time.posixToMillis time)]
                    , resolver = Http.stringResolver apiResolver
                    } ) )

getUser : Model -> Maybe String
getUser m = m.user

logout : Model -> Model
logout m = { m | user = Nothing, pass = Nothing }

login : Model -> String -> String -> Model
login m u p = { m | user = Just u, pass = Just p }

verifyCredentials : (ApiResponse -> msg) -> String -> String -> Cmd msg
verifyCredentials msg user pass =
    let
        m = { init | user = Just user, pass = Just pass }
    in
        case haveValidCredentials m of
            False -> Task.succeed (msg (Err (Http.BadBody "empty credentials"))) |> Task.perform identity
            True -> getBalance m msg

getCaller : Model -> Maybe String
getCaller m = m.caller

getLine : Model -> Maybe String
getLine m = m.line
