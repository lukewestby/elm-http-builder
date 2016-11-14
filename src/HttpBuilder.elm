module HttpBuilder
    exposing
        ( RequestBuilder
        , url
        , get
        , post
        , put
        , patch
        , delete
        , options
        , trace
        , head
        , withHeader
        , withHeaders
        , withBody
        , withStringBody
        , withJsonBody
        , withMultipartBody
        , withMultipartStringBody
        , withUrlEncodedBody
        , withTimeout
        , withCredentials
        , withCacheBuster
        , withZeroStatusAllowed
        , send
        , BodyReader
        , stringReader
        , jsonReader
        , unitReader
        , Error(..)
        , Response
        , toRequest
        , toRequestRecord
        , toSettings
        , Request
        , Settings
        )

{-| Extra helpers for more easily building Http requests that require greater
configuration than what is provided by `elm-http` out of the box.

# Send a request
@docs send

# Start a request
@docs RequestBuilder, url, get, post, put, patch, delete, options, trace, head

# Configure request properties
@docs withHeader, withHeaders, withBody, withStringBody, withJsonBody, withMultipartBody, withMultipartStringBody, withUrlEncodedBody

# Configure settings
@docs withTimeout, withCredentials

# Custom configurations
@docs withCacheBuster, withZeroStatusAllowed

# Parse the response
@docs BodyReader, stringReader, jsonReader, unitReader, Error, Response

# Inspect the request
@docs toRequest, toRequestRecord, toSettings, Request, Settings
-}

-- where

import String
import Task exposing (Task)
import Maybe exposing (Maybe(..))
import Time exposing (Time)
import Json.Decode as JsonDecode
import Json.Encode as JsonEncode
import Dict exposing (Dict)
import Result exposing (Result(Ok, Err))
import Http exposing (Error(..))


{-| A `Request` represents an HTTP request to some url.
-}
type alias Request a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect a
    }


{-| Configurable settings for Requests.
-}
type alias Settings =
    { timeout : Time
    , withCredentials : Bool
    }


{-| By default, don't use a timeout or credentials.
-}
defaultSettings : Settings
defaultSettings =
    { timeout = 0
    , withCredentials = False
    }


type alias Internals =
    { cacheBuster : Bool
    , zeroStatusAllowed : Bool
    }


defaultInternals : Internals
defaultInternals =
    { cacheBuster = False
    , zeroStatusAllowed = False
    }


{-| Construct a url using String, String key value pairs for the query string.
See `Http.url`.

    googleUrl =
        url "https://google.com" [("q", "elm")]
-}
url : String -> List ( String, String ) -> String
url base params =
    if List.isEmpty params then
        base
    else
        base ++ "?" ++ joinUrlEncoded params


{-| A type for chaining request configuration
-}
type RequestBuilder
    = RequestBuilder (Request String) Settings Internals


requestWithVerbAndUrl : String -> String -> RequestBuilder
requestWithVerbAndUrl method url =
    RequestBuilder
        { method = method
        , url = url
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectString
        }
        defaultSettings
        defaultInternals


mapRequest : (Request String -> Request String) -> RequestBuilder -> RequestBuilder
mapRequest updater (RequestBuilder request settings internals) =
    RequestBuilder (updater request)
        (settings)
        (internals)


mapSettings : (Settings -> Settings) -> RequestBuilder -> RequestBuilder
mapSettings updater (RequestBuilder request settings internals) =
    RequestBuilder (request)
        (updater settings)
        (internals)


mapInternals : (Internals -> Internals) -> RequestBuilder -> RequestBuilder
mapInternals updater (RequestBuilder request settings internals) =
    RequestBuilder (request)
        (settings)
        (updater internals)


{-| Start building a GET request with a given URL

    get "https://example.com/api/items/1"
-}
get : String -> RequestBuilder
get =
    requestWithVerbAndUrl "GET"


{-| Start building a POST request with a given URL

    post "https://example.com/api/items"
-}
post : String -> RequestBuilder
post =
    requestWithVerbAndUrl "POST"


{-| Start building a PUT request with a given URL

    put "https://example.com/api/items/1"
-}
put : String -> RequestBuilder
put =
    requestWithVerbAndUrl "PUT"


{-| Start building a PATCH request with a given URL

    patch "https://example.com/api/items/1"
-}
patch : String -> RequestBuilder
patch =
    requestWithVerbAndUrl "PATCH"


{-| Start building a DELETE request with a given URL

    delete "https://example.com/api/items/1"
-}
delete : String -> RequestBuilder
delete =
    requestWithVerbAndUrl "DELETE"


{-| Start building a OPTIONS request with a given URL

    options "https://example.com/api/items/1"
-}
options : String -> RequestBuilder
options =
    requestWithVerbAndUrl "OPTIONS"


{-| Start building a TRACE request with a given URL

    trace "https://example.com/api/items/1"
-}
trace : String -> RequestBuilder
trace =
    requestWithVerbAndUrl "TRACE"


{-| Start building a HEAD request with a given URL

    head "https://example.com/api/items/1"
-}
head : String -> RequestBuilder
head =
    requestWithVerbAndUrl "HEAD"


{-| Add a single header to a request

    get "https://example.com/api/items/1"
        |> withHeader "Content-Type" "application/json"
-}
withHeader : String -> String -> RequestBuilder -> RequestBuilder
withHeader key value =
    mapRequest <| \request -> { request | headers = Http.header key value :: request.headers }


{-| Add many headers to a request

    get "https://example.com/api/items/1"
        |> withHeaders [("Content-Type", "application/json"), ("Accept", "application/json")]
-}
withHeaders : List ( String, String ) -> RequestBuilder -> RequestBuilder
withHeaders headers =
    mapRequest <| \request -> { request | headers = List.map (uncurry Http.header) headers ++ request.headers }


{-| Add a body to a request for requests that allow bodies.

    post "https://example.com/api/items/1"
        |> withHeader "Content-Type" "application/json"
        |> withBody (Http.string """{ "sortBy": "coolness", "take": 10 }""")
-}
withBody : Http.Body -> RequestBuilder -> RequestBuilder
withBody body =
    mapRequest <| \request -> { request | body = body }


{-| Convenience function for adding a string body to a request

    post "https://example.com/api/items/1"
        |> withStringBody "application/json" """{ "sortBy": "coolness", "take": 10 }"""
-}
withStringBody : String -> String -> RequestBuilder -> RequestBuilder
withStringBody contentType data =
    withBody <| Http.stringBody contentType data


{-| Convenience function for adding a JSON body to a request

    params = Json.Encode.object
        [ ("sortBy", Json.Encode.string "coolness")
        , ("take", Json.Encode.int 10)
        ]

    post "https://example.com/api/items/1"
        |> withHeader "Content-Type" "application/json"
        |> withJsonBody params
-}
withJsonBody : JsonEncode.Value -> RequestBuilder -> RequestBuilder
withJsonBody =
    (JsonEncode.encode 0) >> withStringBody "application/json"


{-| Convenience function for adding a multiplart body to a request

    post "https://example.com/api/items/1"
        |> withMultipartBody [Http.stringData "user" (JS.encode user)]
-}
withMultipartBody : List Http.Part -> RequestBuilder -> RequestBuilder
withMultipartBody =
    Http.multipartBody >> withBody


{-| Convience function for adding multipart bodies composed of String, String
key-value pairs. Since `Http.stringData` is currently the only `Http.Data`
creator having this function removes the need to use the `Http.Data` type in
your type signatures.

    post "https://example.com/api/items/1"
        |> withMultipartStringBody [("user", JS.encode user)]
-}
withMultipartStringBody : List ( String, String ) -> RequestBuilder -> RequestBuilder
withMultipartStringBody =
    List.map (\( key, value ) -> Http.stringPart key value)
        >> withMultipartBody


{-| Convenience function for adding url encoded bodies

    post "https://example.com/api/whatever"
        |> withUrlEncodedBody [("user", "Evan"), ("pwd", "secret")]
-}
withUrlEncodedBody : List ( String, String ) -> RequestBuilder -> RequestBuilder
withUrlEncodedBody =
    joinUrlEncoded >> withStringBody "application/x-www-form-urlencoded"


{-| Set the `timeout` setting on the request

    get "https://example.com/api/items/1"
        |> withTimeout (10 * Time.second)
-}
withTimeout : Time -> RequestBuilder -> RequestBuilder
withTimeout timeout =
    mapSettings <| \settings -> { settings | timeout = timeout }


{-| Set the `withCredentials` flag on the request to True. Works via
[`XMLHttpRequest#withCredentials`](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials)

    get "https://example.com/api/items/1"
        |> withCredentials
-}
withCredentials : RequestBuilder -> RequestBuilder
withCredentials =
    mapSettings <| \settings -> { settings | withCredentials = True }


{-| Injects a cache busting url param into the url with the current timestamp as
the value to prevent the server from caching responses
    get "https://example.com/api/items/1"
        |> withCredentials
-}
withCacheBuster : RequestBuilder -> RequestBuilder
withCacheBuster =
    mapInternals <| \internals -> { internals | cacheBuster = True }


{-| Explicitly allows a require for a file:// url with a response status code
of 0 to pass through successfully. This is a common issue when dealing with
file:// urls in environments like cordova, and using this function will allow
you to work around the problem on an opt-in basis.
-}
withZeroStatusAllowed : RequestBuilder -> RequestBuilder
withZeroStatusAllowed =
    mapInternals <| \internals -> { internals | zeroStatusAllowed = True }


{-| Represents a response from the server, including both a decoded JSON payload
and basic network information.
-}
type alias Response a =
    { data : a
    , status : Int
    , statusText : String
    , headers : Dict String String
    , url : String
    }


{-| Decode a Response from an Http.Response
-}
decodeResponse : BodyReader a -> Http.Response String -> Result String (Response a)
decodeResponse reader strResponse =
    let
        replaceBody response data =
            { response | data = data }
    in
        reader strResponse.body
            |> Result.map (replaceBody <| fromResponse strResponse)


{-| Convert a `Http.Response String` into a `Response`, replacing the data.
-}
fromResponse : Http.Response a -> Response a
fromResponse response =
    { data = response.body
    , status = response.status.code
    , statusText = response.status.message
    , headers = response.headers
    , url = response.url
    }


{-| Indicates that _some_ kind of failure occured along the path of making and
receiving the request. This includes a timeout or network issue, a failure to
parse the response body, or a status code outside the 200 range. In the case
that the error is due to a non-2xx response code, the full response is provided
and the data decoded as JSON using the decoder for errors passed to `send`.
-}
type Error a
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus (Response a)
    | BadPayload String (Response a)
    | DecodingFailure String


{-| Parse the `Http.Error` into an `Error a`, optionally allowing for a status
code of `0`.
-}
parseError : BodyReader a -> BodyReader b -> Internals -> Error String -> Task (Error b) (Response a)
parseError successReader errorReader internals error =
    let
        applyOrError : Response String -> (Response b -> Error b) -> Error b
        applyOrError response errorFunc =
            case errorReader response.data of
                Ok decoded ->
                    errorFunc { response | data = decoded }

                Err msg ->
                    DecodingFailure msg
    in
        case error of
            BadStatus resp ->
                if internals.zeroStatusAllowed && resp.status == 0 then
                    case successReader resp.data of
                        Err msg ->
                            Task.fail (DecodingFailure msg)

                        Ok data ->
                            Task.succeed { resp | data = data }
                else
                    Task.fail <| applyOrError resp BadStatus

            BadPayload msg resp ->
                Task.fail <| applyOrError resp <| BadPayload msg

            BadUrl url ->
                Task.fail <| BadUrl url

            Timeout ->
                Task.fail <| Timeout

            NetworkError ->
                Task.fail <| NetworkError

            DecodingFailure msg ->
                Task.fail <| DecodingFailure msg


{-| Convert an `Http.Error` into a String `Error`.
-}
promoteRawError : Http.Error -> Error String
promoteRawError rawError =
    case rawError of
        Http.BadUrl url ->
            BadUrl url

        Http.Timeout ->
            Timeout

        Http.NetworkError ->
            NetworkError

        Http.BadStatus resp ->
            BadStatus <| fromResponse resp

        Http.BadPayload message resp ->
            BadPayload message <| fromResponse resp


{-| A function for transforming raw response bodies into a useful value. Plain
string and JSON decoding readers are provided, and the string reader can be
used as a basis for more custom readers. When future Http value types become
supported matching readers will be added to extract them.
-}
type alias BodyReader a =
    String -> Result String a


{-| Attempts to read a raw response body as a plain text string, failing if the
body is not readable as a string.
-}
stringReader : BodyReader String
stringReader value =
    Ok value


{-| Attempts to decode the raw response body with the given
`Json.Decode.Decoder`, failing if the body is malformed or not readable as a
string.
-}
jsonReader : JsonDecode.Decoder a -> BodyReader a
jsonReader =
    JsonDecode.decodeString


{-| Totally discards the content of a raw response body and reads nothing,
returning `()` as the data value. Great for discarding error response bodies if
they just look like `"Not Found"` or whatever and aren't really useful.
-}
unitReader : BodyReader ()
unitReader =
    always <| Ok ()


{-| Once you're finished building up a request, send it with readers for the
successful response value as well as the server error response value.

    -- In this example a succesful response from the server looks like
    -- ["string", "string", "string"], and an error body might look like
    -- "Bad Request" or something similar, such that it is a string that is
    -- not valid JSON (it would need to look like "\"Bad Request\"" to be
    -- decodable as JSON).

    successDecoder : Json.Decode.Decoder (List String)
    successDecoder =
        Json.Decode.list Json.Decode.string

    get "https://example.com/api/items"
        |> withHeader "Content-Type" "application/json"
        |> withTimeout (10 * Time.second)
        |> send (jsonReader successDecoder) stringReader
-}
send : BodyReader a -> BodyReader b -> RequestBuilder -> Task (Error b) (Response a)
send successReader errorReader (RequestBuilder request settings internals) =
    if internals.cacheBuster then
        Time.now
            |> Task.map (appendCacheBusterToUrl request)
            |> Task.andThen (sendHelp successReader errorReader internals settings)
    else
        sendHelp successReader errorReader internals settings request


sendHelp :
    BodyReader a
    -> BodyReader b
    -> Internals
    -> Settings
    -> Request String
    -> Task (Error b) (Response a)
sendHelp successReader errorReader internals settings request =
    toRequest (RequestBuilder request settings internals) successReader
        |> Http.toTask
        |> Task.mapError promoteRawError
        |> Task.onError (parseError successReader errorReader internals)


appendCacheBusterToUrl : Request String -> Time -> Request String
appendCacheBusterToUrl request time =
    { request | url = appendQuery request.url "_" (toString time) }


{-| Extract the Http.Request component of the builder, for introspection and
testing
-}
toRequest : RequestBuilder -> BodyReader a -> Http.Request (Response a)
toRequest builder reader =
    toRequestRecord reader builder |> Http.request


{-| Extract a record used to create an Http.Request from the HttpBuilder.
-}
toRequestRecord :
    BodyReader a
    -> RequestBuilder
    -> { body : Http.Body
       , expect : Http.Expect (Response a)
       , headers : List Http.Header
       , method : String
       , timeout : Maybe Time
       , url : String
       , withCredentials : Bool
       }
toRequestRecord reader (RequestBuilder request settings internals) =
    { method = request.method
    , headers = request.headers
    , url = request.url
    , body = request.body
    , expect = Http.expectStringResponse (decodeResponse reader)
    , timeout = Just settings.timeout
    , withCredentials = settings.withCredentials
    }


{-| Extract the Http.Settings component of the builder, for introspection and
testing
-}
toSettings : RequestBuilder -> Settings
toSettings (RequestBuilder request settings internals) =
    settings


joinUrlEncoded : List ( String, String ) -> String
joinUrlEncoded args =
    String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


queryEscape : String -> String
queryEscape =
    Http.encodeUri >> replace "%20" "+"


replace : String -> String -> String -> String
replace old new =
    String.split old >> String.join new


appendQuery : String -> String -> String -> String
appendQuery url key value =
    if String.contains "?" url then
        url ++ "&" ++ key ++ "=" ++ value
    else
        url ++ "?" ++ key ++ "=" ++ value
