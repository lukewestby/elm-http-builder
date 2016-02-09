module Http.Extra
  ( RequestBuilder, url, get, post, put, patch, delete
  , withHeader, withHeaders, withBody, withStringBody, withMultipartBody, withMultipartStringBody
  , withTimeout, withStartHandler, withProgressHandler, withMimeType, withCredentials
  , send, Error(..), Response
  , toRequest, toSettings, Request, Settings
  ) where

{-| Extra helpers for more easily building Http requests that require greater
configuration than what is provided by `elm-http` out of the box.


# Start a request
@docs RequestBuilder, url, get, post, put, patch, delete

# Configure request properties
@docs withHeader, withHeaders, withBody, withStringBody, withMultipartBody, withMultipartStringBody

# Configure settings
@docs withTimeout, withStartHandler, withProgressHandler, withMimeType, withCredentials

# Send the request
@docs send, Error, Response

# Inspect the request
@docs toRequest, toSettings, Request, Settings
-}

import Task exposing (Task)
import Maybe exposing (Maybe(..))
import Time exposing (Time)
import Json.Decode as Json
import Dict exposing (Dict)
import Result exposing (Result(Ok, Err))
import Http exposing (Value(Text), RawError(..))


{-| Re-export `Http.Request`
-}
type alias Request =
  Http.Request


{-| Re-export `Http.Settings`
-}
type alias Settings =
  Http.Settings


{-| Construct a url using String, String key value pairs for the query string.
See `Http.url`.

    googleUrl =
      url "https://google.com" [("q", "elm")]
-}
url : String -> List (String, String) -> String
url =
  Http.url


{-| A type for chaining request configuration
-}
type RequestBuilder =
  RequestBuilder Http.Request Http.Settings


requestWithVerbAndUrl : String -> String -> RequestBuilder
requestWithVerbAndUrl verb url =
  RequestBuilder
    { verb = verb
    , url = url
    , headers = []
    , body = Http.empty
    }
    Http.defaultSettings


mapRequest : (Http.Request -> Http.Request) -> RequestBuilder -> RequestBuilder
mapRequest updater (RequestBuilder request settings) =
  RequestBuilder
    (updater request)
    (settings)


mapSettings : (Http.Settings -> Http.Settings) -> RequestBuilder -> RequestBuilder
mapSettings updater (RequestBuilder request settings) =
  RequestBuilder
    (request)
    (updater settings)


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

    post "https://example.com/api/items/1"
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


{-| Add a single header to a request

    get "https://example.com/api/items/1"
      |> withHeader ("Content-Type", "application/json")
-}
withHeader : (String, String) -> RequestBuilder -> RequestBuilder
withHeader header =
  mapRequest <| \request -> { request | headers = header :: request.headers }


{-| Add many headers to a request

    get "https://example.com/api/items/1"
      |> withHeaders [("Content-Type", "application/json"), ("Accept", "application/json")]
-}
withHeaders : List (String, String) -> RequestBuilder -> RequestBuilder
withHeaders headers =
  mapRequest <| \request -> { request | headers = headers ++ request.headers }


{-| Add a body to a request for requests that allow bodies.

    post "https://example.com/api/items/1"
      |> withHeader ("Content-Type", "application/json")
      |> withBody (Http.string """{ "sortBy": "coolness", "take": 10 }""")
-}
withBody : Http.Body -> RequestBuilder -> RequestBuilder
withBody body =
  mapRequest <| \request -> { request | body = body }


{-| Convenience function for adding a string body to a request

    post "https://example.com/api/items/1"
      |> withHeader ("Content-Type", "application/json")
      |> withStringBody """{ "sortBy": "coolness", "take": 10 }"""
-}
withStringBody : String -> RequestBuilder -> RequestBuilder
withStringBody content =
  withBody (Http.string content)


{-| Convenience function for adding a multiplart body to a request

      post "https://example.com/api/items/1"
        |> withMultipartBody [Http.stringData "user" (JS.encode user)]
-}
withMultipartBody : List Http.Data -> RequestBuilder -> RequestBuilder
withMultipartBody components =
  withBody (Http.multipart components)


{-| Convience function for adding multipart bodies composed of String, String
key-value pairs. Since `Http.stringData` is currently the only `Http.Data`
creator having this function removes the need to use the `Http.Data` type in
your type signatures.

    post "https://example.com/api/items/1"
      |> withMultipartStringBody [("user", JS.encode user)]
-}
withMultipartStringBody : List (String, String) -> RequestBuilder -> RequestBuilder
withMultipartStringBody =
  List.map (\(key, value) -> Http.stringData key value)
    >> withMultipartBody


{-| Set the `timeout` setting on the request

    get "https://example.com/api/items/1"
      |> withTimeout (10 * Time.second)
-}
withTimeout : Time -> RequestBuilder -> RequestBuilder
withTimeout timeout =
  mapSettings <| \settings -> { settings | timeout = timeout }


{-| Set the `onStart` setting on the request

    get "https://example.com/api/items/1"
      |> withStartHandler (onStartTask)
-}
withStartHandler : Task () () -> RequestBuilder -> RequestBuilder
withStartHandler task =
  mapSettings <| \settings -> { settings | onStart = Just task }


{-| Set the `onProgress` setting on the request

    get "https://example.com/api/items/1"
      |> withProgressHandler (onProgressHandler)
-}
withProgressHandler : (Maybe { loaded : Int, total : Int } -> Task () ()) -> RequestBuilder -> RequestBuilder
withProgressHandler progressHandler =
  mapSettings <| \settings -> { settings | onProgress = Just progressHandler }


{-| Set the desired type of the response for the request, works via
[`XMLHttpRequest#overrideMimeType()`](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#overrideMimeType())

    get "https://example.com/api/items/1"
      |> withMimeType (onProgressHandler)
-}
withMimeType : String -> RequestBuilder -> RequestBuilder
withMimeType mimeType =
  mapSettings <| \settings -> { settings | desiredResponseType = Just mimeType }


{-| Set the `withCredentials` flag on the request to True. Works via
[`XMLHttpRequest#withCredentials`](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials)

    get "https://example.com/api/items/1"
      |> withCredentials
-}
withCredentials : RequestBuilder -> RequestBuilder
withCredentials =
  mapSettings <| \settings -> { settings | withCredentials = True }


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


{-| Indicates that _some_ kind of failure occured along the path of making and
receiving the request. This includes a timeout or network issue, a failure to
parse the response body, or a status code outside the 200 range. In the case
that the error is due to a non-2xx response code, the full response is provided
and the data decoded as JSON using the decoder for errors passed to `send`.
-}
type Error a
  = UnexpectedPayload String
  | NetworkError
  | Timeout
  | BadResponse (Response a)


{-| Once you're finished building up a request, send it with decoders for the
successful response object as well as the server error response object

    successDecoder : Json.Decode.Decoder (List String)
    successDecoder =
      Json.Decode.list Json.Decode.string

    errorDecoder : Json.Decode.Decoder String)
    errorDecoder =
      Json.Decode.string

    get "https://example.com/api/items"
      |> withHeader ("Content-Type", "application/json")
      |> withTimeout (10 * Time.second)
      |> send successDecoder errorDecoder
-}
send : Json.Decoder a -> Json.Decoder b -> RequestBuilder -> Task (Error b) (Response a)
send successDecoder errorDecoder (RequestBuilder request settings) =
  Http.send settings request
    |> Task.mapError promoteRawError
    |> (flip Task.andThen) (handleResponse successDecoder errorDecoder)


promoteRawError : Http.RawError -> Error a
promoteRawError rawError =
  case rawError of
    RawTimeout ->
      Timeout
    RawNetworkError ->
      NetworkError


responseFromRaw : Json.Decoder a -> Http.Response -> Task (Error b) (Response a)
responseFromRaw decoder response =
  case response.value of
    Text str ->
      case Json.decodeString decoder str of
        Ok data ->
          Task.succeed
            { data = data
            , status = response.status
            , statusText = response.statusText
            , headers = response.headers
            , url = response.url
            }

        Err message ->
          Task.fail (UnexpectedPayload message)
    _ ->
      Task.fail (UnexpectedPayload "Response body types other than string not supported.")


handleResponse : Json.Decoder a -> Json.Decoder b -> Http.Response -> Task (Error b) (Response a)
handleResponse successDecoder errorDecoder response =
  let
    isSuccessful =
      response.status >= 200 && response.status < 300
  in
    if isSuccessful then
      responseFromRaw successDecoder response
    else
      responseFromRaw errorDecoder response
        |> (flip Task.andThen) (BadResponse >> Task.fail)



{-| Extract the Http.Request component of the builder, for introspection and
testing
-}
toRequest : RequestBuilder -> Request
toRequest (RequestBuilder request settings) =
  request


{-| Extract the Http.Settings component of the builder, for introspection and
testing
-}
toSettings : RequestBuilder -> Settings
toSettings (RequestBuilder request settings) =
  settings
