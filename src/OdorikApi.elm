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
import Task
import Time
import Url.Builder as UB

type alias Model =
    { user : String
    , pass : String
    , line : String -- technically int, but meh
    , caller : String
    }

type alias ApiResponse =
    Result Http.Error String

init : Model
init =
    { user = ""
    , pass = ""
    , line = "111"
    , caller = "+420 800 123 456"
    }

decoder : Json.Decoder Model
decoder =
    Json.map4 Model
        (Json.field "user" Json.string)
        (Json.field "pass" Json.string)
        (Json.field "line" Json.string)
        (Json.field "caller" Json.string)

encoder : Model -> Encode.Value
encoder m =
    Encode.object
        [ ("user", Encode.string m.user)
        , ("pass", Encode.string m.pass)
        , ("line", Encode.string m.line)
        , ("caller", Encode.string m.caller)
        ]

serverFromCreds : Model -> String
serverFromCreds m =
    case (m.user, m.pass) of
        ("888", "888") -> "https://wejn.com/odorik_test_api___"
        _ -> "https://www.odorik.cz/api/v1"

apiUrlFromCreds : Model -> String -> List (UB.QueryParameter) -> String
apiUrlFromCreds m api qp =
    UB.custom (UB.CrossOrigin (serverFromCreds m)) [api] ([UB.string "user" m.user, UB.string "password" m.pass] ++ qp) Nothing

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
    not ((String.isEmpty m.user) || (String.isEmpty m.pass))

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

getUser : Model -> String
getUser m = m.user

logout : Model -> Model
logout m = { m | user = "", pass = "" }

login : Model -> String -> String -> Model
login m u p = { m | user = u, pass = p }

verifyCredentials : (ApiResponse -> msg) -> String -> String -> Cmd msg
verifyCredentials msg user pass =
    let
        m = { init | user = user, pass = pass }
    in
        case haveValidCredentials m of
            False -> Task.succeed (msg (Err (Http.BadBody "empty credentials"))) |> Task.perform identity
            True -> getBalance m msg

getCaller : Model -> String
getCaller m = m.caller

getLine : Model -> String
getLine m = m.line
