module HttpBuilder
    exposing
        ( RequestBuilder
        , delete
        , get
        , head
        , options
        , patch
        , post
        , put
        , requestUrl
        , send
        , toRequest
        , trace
        , withBearerToken
        , withBody
        , withCacheBuster
        , withCredentials
        , withExpect
        , withExpectJson
        , withExpectString
        , withHeader
        , withHeaders
        , withJsonBody
        , withMultipartStringBody
        , withQueryParam
        , withQueryParams
        , withStringBody
        , withTimeout
        , withUrlEncodedBody
        )

{-| Extra helpers for more easily building Http requests that require greater
configuration than what is provided by `elm-http` out of the box.


# Start a request

@docs RequestBuilder, get, post, put, patch, delete, options, trace, head


# Configure request properties

@docs withHeader, withHeaders, withBody, withStringBody, withJsonBody
@docs withMultipartStringBody, withUrlEncodedBody, withTimeout, withCredentials
@docs withQueryParam, withQueryParams, withExpect, withCacheBuster, withExpectJson
@docs withExpectString, withBearerToken


# Make the request

@docs toRequest, send


# Retreive data about the request

@docs requestUrl

-}

-- where

import Http
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import String
import Task exposing (Task)
import Time
import Url

{-| A type for chaining request configuration
-}
type alias RequestBuilder msg =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect msg
    , timeout : Maybe Float
    , withCredentials : Bool
    , queryParams : List ( String, String )
    , cacheBuster : Maybe String
    }


requestWithMethodAndUrl : String -> String -> RequestBuilder ()
requestWithMethodAndUrl method url =
    { method = method
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectString (\_ -> ())
    , timeout = Nothing
    , withCredentials = False
    , queryParams = []
    , cacheBuster = Nothing
    }


{-| Start building a GET request with a given URL

    get "https://example.com/api/items/1"

-}
get : String -> RequestBuilder ()
get =
    requestWithMethodAndUrl "GET"


{-| Start building a POST request with a given URL

    post "https://example.com/api/items"

-}
post : String -> RequestBuilder ()
post =
    requestWithMethodAndUrl "POST"


{-| Start building a PUT request with a given URL

    put "https://example.com/api/items/1"

-}
put : String -> RequestBuilder ()
put =
    requestWithMethodAndUrl "PUT"


{-| Start building a PATCH request with a given URL

    patch "https://example.com/api/items/1"

-}
patch : String -> RequestBuilder ()
patch =
    requestWithMethodAndUrl "PATCH"


{-| Start building a DELETE request with a given URL

    delete "https://example.com/api/items/1"

-}
delete : String -> RequestBuilder ()
delete =
    requestWithMethodAndUrl "DELETE"


{-| Start building a OPTIONS request with a given URL

    options "https://example.com/api/items/1"

-}
options : String -> RequestBuilder ()
options =
    requestWithMethodAndUrl "OPTIONS"


{-| Start building a TRACE request with a given URL

    trace "https://example.com/api/items/1"

-}
trace : String -> RequestBuilder ()
trace =
    requestWithMethodAndUrl "TRACE"


{-| Start building a HEAD request with a given URL

    head "https://example.com/api/items/1"

-}
head : String -> RequestBuilder ()
head =
    requestWithMethodAndUrl "HEAD"


{-| Add a single header to a request

    get "https://example.com/api/items/1"
        |> withHeader "Content-Type" "application/json"

-}
withHeader : String -> String -> RequestBuilder msg -> RequestBuilder msg
withHeader key value builder =
    { builder | headers = Http.header key value :: builder.headers }


{-| Add many headers to a request

    get "https://example.com/api/items/1"
        |> withHeaders [("Content-Type", "application/json"), ("Accept", "application/json")]

-}
withHeaders : List ( String, String ) -> RequestBuilder msg -> RequestBuilder msg
withHeaders headerPairs builder =
    { builder
        | headers = List.map (\( key, value ) -> Http.header key value) headerPairs ++ builder.headers
    }


{-| Add a bearer token to a request

    get "https://example.com/api/items/1"
        |> withBearerToken "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhIjoiYSJ9.MvhYYpYBuN1rUaV0GGnQGvr889zY0xSc20Lnt8nMTfE"

-}
withBearerToken : String -> RequestBuilder msg -> RequestBuilder msg
withBearerToken value builder =
    { builder | headers = Http.header "Authorization" ("Bearer " ++ value) :: builder.headers }


{-| Add an Http.Body to the request

    post "https://example.com/api/save-text"
        |> withBody (Http.stringBody "text/plain" "Hello!")

-}
withBody : Http.Body -> RequestBuilder msg -> RequestBuilder msg
withBody body builder =
    { builder | body = body }


{-| Convenience function for adding a string body to a request

    post "https://example.com/api/items/1"
        |> withStringBody "application/json" """{ "sortBy": "coolness", "take": 10 }"""

-}
withStringBody : String -> String -> RequestBuilder msg -> RequestBuilder msg
withStringBody contentType value =
    withBody <| Http.stringBody contentType value


{-| Convenience function for adding a JSON body to a request

    params = Json.Encode.object
        [ ("sortBy", Json.Encode.string "coolness")
        , ("take", Json.Encode.int 10)
        ]

    post "https://example.com/api/items/1"
        |> withJsonBody params

-}
withJsonBody : Encode.Value -> RequestBuilder msg -> RequestBuilder msg
withJsonBody value =
    withBody <| Http.jsonBody value


{-| Convenience function for adding multipart bodies composed of String, String
key-value pairs. Since `Http.stringData` is currently the only `Http.Data`
creator having this function removes the need to use the `Http.Data` type in
your type signatures.

    post "https://example.com/api/items/1"
        |> withMultipartStringBody [("user", JS.encode user)]

-}
withMultipartStringBody : List ( String, String ) -> RequestBuilder msg -> RequestBuilder msg
withMultipartStringBody partPairs =
    withBody <| Http.multipartBody <| List.map (\( key, value ) -> Http.stringPart key value) partPairs


{-| Convenience function for adding url encoded bodies

    post "https://example.com/api/whatever"
        |> withUrlEncodedBody [("user", "Luke"), ("pwd", "secret")]

-}
withUrlEncodedBody : List ( String, String ) -> RequestBuilder msg -> RequestBuilder msg
withUrlEncodedBody =
    joinUrlEncoded >> withStringBody "application/x-www-form-urlencoded"


{-| Set the `timeout` setting on the request

    get "https://example.com/api/items/1"
        |> withTimeout (10 * Time.second)

-}
withTimeout : Float -> RequestBuilder msg -> RequestBuilder msg
withTimeout timeout builder =
    { builder | timeout = Just timeout }


{-| Set the `withCredentials` flag on the request to True. Works via
[`XMLHttpRequest#withCredentials`](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials)

    get "https://example.com/api/items/1"
        |> withCredentials

-}
withCredentials : RequestBuilder msg -> RequestBuilder msg
withCredentials builder =
    { builder | withCredentials = True }


{-| Choose an `Expect` for the request

    get "https://example.com/api/items/1"
        |> withExpect (Http.expectJson GotItem itemsDecoder)

-}
withExpect : Http.Expect b -> RequestBuilder msg -> RequestBuilder b
withExpect expect builder =
    { method = builder.method
    , headers = builder.headers
    , url = builder.url
    , body = builder.body
    , timeout = builder.timeout
    , withCredentials = builder.withCredentials
    , queryParams = builder.queryParams
    , cacheBuster = builder.cacheBuster
    , expect = expect
    }


{-| Choose a Json `Expect` for the request

    get "https://example.com/api/items/1"
        |> withExpectJson GotItem itemsDecoder

-}
withExpectJson : (Result Http.Error a -> msg) -> Decoder a -> RequestBuilder b -> RequestBuilder msg
withExpectJson toMsg decoder builder =
    { method = builder.method
    , headers = builder.headers
    , url = builder.url
    , body = builder.body
    , timeout = builder.timeout
    , withCredentials = builder.withCredentials
    , queryParams = builder.queryParams
    , cacheBuster = builder.cacheBuster
    , expect = Http.expectJson toMsg decoder
    }


{-| Choose a String `Expect` for the request

    get "https://example.com/api/items/1"
        |> withExpectString GotItem

-}
withExpectString : (Result Http.Error String -> msg) -> RequestBuilder b -> RequestBuilder msg
withExpectString toMsg builder =
    { method = builder.method
    , headers = builder.headers
    , url = builder.url
    , body = builder.body
    , timeout = builder.timeout
    , withCredentials = builder.withCredentials
    , queryParams = builder.queryParams
    , cacheBuster = builder.cacheBuster
    , expect = Http.expectString toMsg
    }


{-| Add a query param to the url for the request

    get "https://example.com/api/items/1"
        |> withQueryParam "hello" "world"
    -- sends a request to https://example.com/api/items/1?hello=world

-}
withQueryParam : String -> String -> RequestBuilder msg -> RequestBuilder msg
withQueryParam key value builder =
    withQueryParams [ ( key, value ) ] builder


{-| Add some query params to the url for the request

    get "https://example.com/api/items/1"
        |> withQueryParams [("hello", "world"), ("foo", "bar")]
        |> withQueryParams [("baz", "qux")]
    -- sends a request to https://example.com/api/items/1?hello=world&foo=bar&baz=qux

-}
withQueryParams : List ( String, String ) -> RequestBuilder msg -> RequestBuilder msg
withQueryParams queryParams builder =
    { builder | queryParams = builder.queryParams ++ queryParams }


{-| Send the request with a Time based cache buster added to the URL.
You provide a key for an extra query param, and when the request is sent that
query param will be given a value with the current timestamp.

    type Msg
        = Items (Result Http.Error String)

    get "https://example.com/api/items"
        |> withExpect (Http.expectString)
        |> withCacheBuster "cache_buster"
        |> send Items

    -- makes a request to https://example.com/api/items?cache_buster=1481633217383

-}
withCacheBuster : String -> RequestBuilder msg -> RequestBuilder msg
withCacheBuster paramName builder =
    { builder | cacheBuster = Just paramName }


{-| Convert the RequestBuilder to a string representation of the URL
with all options applied.
-}
requestUrl : RequestBuilder msg -> String
requestUrl builder =
    let
        encodedParams =
            joinUrlEncoded builder.queryParams

        fullUrl =
            if String.isEmpty encodedParams then
                builder.url
            else
                builder.url ++ "?" ++ encodedParams
    in
        fullUrl


{-| Extract the Http.Request component of the builder in case you want to use it
directly. **This function is lossy** and will discard some of the extra stuff
that HttpBuilder allows you to do.

Things that will be lost:

  - Attaching a cache buster to requests using `withCacheBuster`

-}
toRequest : RequestBuilder msg -> Cmd msg
toRequest builder =
    let req = if builder.withCredentials
              then Http.riskyRequest
              else Http.request

    in req
        { method = builder.method
        , url = requestUrl builder
        , headers = builder.headers
        , body = builder.body
        , expect = builder.expect
        , timeout = builder.timeout
        , tracker = Nothing
        }


{-| Send the request
-}
send : RequestBuilder msg -> Cmd msg
send builder =
    toRequest builder


joinUrlEncoded : List ( String, String ) -> String
joinUrlEncoded args =
    String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


queryEscape : String -> String
queryEscape =
    Url.percentEncode >> replace "%20" "+"


replace : String -> String -> String -> String
replace old new =
    String.split old >> String.join new
