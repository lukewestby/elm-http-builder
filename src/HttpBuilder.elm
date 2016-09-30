module HttpBuilder
    exposing
        ( RequestBuilder
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
        , withStringBody
        , withJsonBody
        , withFormDataBody
        , withUrlEncodedBody
        , withTimeout
        , withCredentials
        , withSuccessExpectation
        , withErrorExpectation
        , send
        , toRequest
        )

{-| Extra helpers for more easily building Http requests that require greater
configuration than what is provided by `elm-http` out of the box.

# Start a request
@docs RequestBuilder, get, post, put, patch, delete, options, trace, head

# Configure request properties
@docs withHeader, withHeaders, withStringBody, withJsonBody, withFormDataBody, withUrlEncodedBody, withTimeout, withCredentials

# Read a response
@docs withSuccessExpectation, withErrorExpectation

# Send a request
@docs send, toRequest

-}

import Time exposing (Time)
import Json.Encode exposing (Value)
import Http exposing (Request, Header, header, Part, stringPart, Body, emptyBody, stringBody, jsonBody, multipartBody, Expect, expectNothing, Error)


-- PUBLIC API


{-| A type for chaining request configuration
-}
type RequestBuilder x a
    = RequestBuilder
        { method : String
        , url : String
        , headers : List Header
        , body : Body
        , expectOnSuccess : Expect a
        , expectOnError : Expect x
        , timeout : Maybe Time
        , withCredentials : Bool
        }


{-| Start building a GET request with a given URL

    get "https://example.com/api/items/1"
-}
get : String -> RequestBuilder () ()
get =
    requestWithMethodAndUrl "GET"


{-| Start building a POST request with a given URL

    post "https://example.com/api/items"
-}
post : String -> RequestBuilder () ()
post =
    requestWithMethodAndUrl "POST"


{-| Start building a PUT request with a given URL

    put "https://example.com/api/items/1"
-}
put : String -> RequestBuilder () ()
put =
    requestWithMethodAndUrl "PUT"


{-| Start building a PATCH request with a given URL

    patch "https://example.com/api/items/1"
-}
patch : String -> RequestBuilder () ()
patch =
    requestWithMethodAndUrl "PATCH"


{-| Start building a DELETE request with a given URL

    delete "https://example.com/api/items/1"
-}
delete : String -> RequestBuilder () ()
delete =
    requestWithMethodAndUrl "DELETE"


{-| Start building a OPTIONS request with a given URL

    options "https://example.com/api/items/1"
-}
options : String -> RequestBuilder () ()
options =
    requestWithMethodAndUrl "OPTIONS"


{-| Start building a TRACE request with a given URL

    trace "https://example.com/api/items/1"
-}
trace : String -> RequestBuilder () ()
trace =
    requestWithMethodAndUrl "TRACE"


{-| Start building a HEAD request with a given URL

    head "https://example.com/api/items/1"
-}
head : String -> RequestBuilder () ()
head =
    requestWithMethodAndUrl "HEAD"


{-| Add a single header to a request

    get "https://example.com/api/items/1"
        |> withHeader "Content-Type" "application/json"
-}
withHeader : String -> String -> RequestBuilder x a -> RequestBuilder x a
withHeader key value =
    map <| \details -> { details | headers = (header key value) :: request.headers }


{-| Add many headers to a request

    get "https://example.com/api/items/1"
        |> withHeaders [("Content-Type", "application/json"), ("Accept", "application/json")]
-}
withHeaders : List ( String, String ) -> RequestBuilder x a -> RequestBuilder x a
withHeaders headers =
    let
        additionalHeaders =
            List.map (uncurry header) headers
    in
        map <| \details -> { details | headers = additionalHeaders ++ request.headers }


{-| Convenience function for adding a string body to a request

    post "https://example.com/api/items/1"
        |> withHeader "Content-Type" "application/json"
        |> withStringBody """{ "sortBy": "coolness", "take": 10 }"""
-}
withStringBody : String -> String -> RequestBuilder x a -> RequestBuilder x a
withStringBody mimeType data builder =
    withBody (stringBody mimeType data) builder


{-| Convenience function for adding a JSON body to a request

    params = Json.Encode.object
        [ ("sortBy", Json.Encode.string "coolness")
        , ("take", Json.Encode.int 10)
        ]

    post "https://example.com/api/items/1"
        |> withHeader "Content-Type" "application/json"
        |> withJsonBody params
-}
withJsonBody : Value -> RequestBuilder x a -> RequestBuilder x a
withJsonBody value builder =
    withBody (jsonBody value) builder


{-| Convience function for adding multipart bodies composed of String, String
key-value pairs. Since `Http.stringData` is currently the only `Http.Data`
creator having this function removes the need to use the `Http.Data` type in
your type signatures.

    post "https://example.com/api/items/1"
        |> withMultipartStringBody [("user", JS.encode user)]
-}
withFormDataBody : List ( String, String ) -> RequestBuilder x a -> RequestBuilder x a
withFormDataBody entries builder =
    let
        formData =
            List.map (uncurry stringPart) entries |> multipartBody
    in
        withBody formData builder


{-| Convenience function for adding url encoded bodies

    post "https://example.com/api/whatever"
        |> withUrlEncodedBody [("user", "Evan"), ("pwd", "secret")]
-}
withUrlEncodedBody : List ( String, String ) -> RequestBuilder x a -> RequestBuilder x a
withUrlEncodedBody entries builder =
    withStringBody "application/x-www-form-urlencoded" (joinUrlEncoded entries) builder


{-| Set the `timeout` setting on the request

    get "https://example.com/api/items/1"
        |> withTimeout (10 * Time.second)
-}
withTimeout : Time -> RequestBuilder x a -> RequestBuilder x a
withTimeout timeout =
    map <| \details -> { details | timeout = Just timeout }


{-| Set the `withCredentials` flag on the request to True. Works via
[`XMLHttpRequest#withCredentials`](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials)

    get "https://example.com/api/items/1"
        |> withCredentials
-}
withCredentials : RequestBuilder x a -> RequestBuilder x a
withCredentials =
    map <| \details -> { details | withCredentials = True }


withSuccessExpectation : Expect b -> RequestBuilder x a -> RequestBuilder x b
withSuccessExpectation expectOnSuccess =
    map <| \details -> { details | expectOnSuccess = expectOnSuccess }


withErrorExpectation : Expect y -> RequestBuilder x a -> RequestBuilder y a
withErrorExpectation expectOnError =
    map <| \details -> { details | expectOnError = expectOnError }


toRequest : RequestBuilder x a -> Request x a
toRequest (RequestBuilder details) =
    Http.toRequest details


send : (Result (Error x) a -> msg) -> RequestBuilder x a -> Cmd msg
send tagger builder
    builder |> toRequest |> Http.send tagger



-- INTERNAL STUFF


requestWithMethodAndUrl : String -> String -> RequestBuilder () ()
requestWithMethodAndUrl method url =
    RequestBuilder
        { method = method
        , url = url
        , headers = []
        , body = emptyBody
        , expectOnSuccess = expectNothing
        , expectOnError = expectNothing
        , timeout = Nothing
        , withCredentials = False
        }


map fn (RequestBuilder details) =
    RequestBuilder <| fn details


withBody : Body -> RequestBuilder -> RequestBuilder
withBody body =
    map <| \details -> { details | body = body }


joinUrlEncoded : List ( String, String ) -> String
joinUrlEncoded args =
    String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


queryEscape : String -> String
queryEscape =
    Http.uriEncode >> replace "%20" "+"


replace : String -> String -> String -> String
replace old new =
    String.split old >> String.join new


appendQuery : String -> String -> String -> String
appendQuery url key value =
    if String.contains "?" url then
        url ++ "&" ++ key ++ "=" ++ value
    else
        url ++ "?" ++ key ++ "=" ++ value
