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
        , withMultipartStringBody
        , withUrlEncodedBody
        , withTimeout
        , withCredentials
        , withQueryParams
        , withExpect
        , toRequest
        , send
        )

{-| Extra helpers for more easily building Http requests that require greater
configuration than what is provided by `elm-http` out of the box.

# Start a request
@docs RequestBuilder, get, post, put, patch, delete, options, trace, head

# Configure request properties
@docs withHeader, withHeaders, withStringBody, withJsonBody, withMultipartStringBody, withUrlEncodedBody, withTimeout, withCredentials, withQueryParams, withExpect

# Make the request
@docs toRequest, send
-}

-- where

import String
import Task exposing (Task)
import Maybe exposing (Maybe(..))
import Time exposing (Time)
import Json.Decode as Decode
import Json.Encode as Encode
import Dict exposing (Dict)
import Result exposing (Result(Ok, Err))
import Http


type alias RequestDetails a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Time
    , withCredentials : Bool
    , queryParams : List ( String, String )
    }


{-| A type for chaining request configuration
-}
type RequestBuilder a
    = RequestBuilder (RequestDetails a)


requestWithMethodAndUrl : String -> String -> RequestBuilder ()
requestWithMethodAndUrl method url =
    RequestBuilder
        { method = method
        , url = url
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        , queryParams = []
        }


map : (RequestDetails a -> RequestDetails b) -> RequestBuilder a -> RequestBuilder b
map fn (RequestBuilder details) =
    RequestBuilder <| fn details


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
withHeader : String -> String -> RequestBuilder a -> RequestBuilder a
withHeader key value =
    map <| \details -> { details | headers = (Http.header key value) :: details.headers }


{-| Add many headers to a request

    get "https://example.com/api/items/1"
        |> withHeaders [("Content-Type", "application/json"), ("Accept", "application/json")]
-}
withHeaders : List ( String, String ) -> RequestBuilder a -> RequestBuilder a
withHeaders headerPairs =
    map <|
        \details ->
            { details
                | headers = (List.map (uncurry Http.header) headerPairs) ++ details.headers
            }


withBody : Http.Body -> RequestBuilder a -> RequestBuilder a
withBody body =
    map <| \details -> { details | body = body }


{-| Convenience function for adding a string body to a request

    post "https://example.com/api/items/1"
        |> withStringBody "application/json" """{ "sortBy": "coolness", "take": 10 }"""
-}
withStringBody : String -> String -> RequestBuilder a -> RequestBuilder a
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
withJsonBody : Encode.Value -> RequestBuilder a -> RequestBuilder a
withJsonBody value =
    withBody <| Http.jsonBody value


{-| Convience function for adding multipart bodies composed of String, String
key-value pairs. Since `Http.stringData` is currently the only `Http.Data`
creator having this function removes the need to use the `Http.Data` type in
your type signatures.

    post "https://example.com/api/items/1"
        |> withMultipartStringBody [("user", JS.encode user)]
-}
withMultipartStringBody : List ( String, String ) -> RequestBuilder a -> RequestBuilder a
withMultipartStringBody partPairs =
    map <|
        \details ->
            { details
                | body = Http.multipartBody <| List.map (uncurry Http.stringPart) partPairs
            }


{-| Convenience function for adding url encoded bodies

    post "https://example.com/api/whatever"
        |> withUrlEncodedBody [("user", "Luke"), ("pwd", "secret")]
-}
withUrlEncodedBody : List ( String, String ) -> RequestBuilder a -> RequestBuilder a
withUrlEncodedBody =
    joinUrlEncoded >> withStringBody "application/x-www-form-urlencoded"


{-| Set the `timeout` setting on the request

    get "https://example.com/api/items/1"
        |> withTimeout (10 * Time.second)
-}
withTimeout : Time -> RequestBuilder a -> RequestBuilder a
withTimeout timeout =
    map <| \details -> { details | timeout = Just timeout }


{-| Set the `withCredentials` flag on the request to True. Works via
[`XMLHttpRequest#withCredentials`](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials)

    get "https://example.com/api/items/1"
        |> withCredentials
-}
withCredentials : RequestBuilder a -> RequestBuilder a
withCredentials =
    map <| \details -> { details | withCredentials = True }


{-| Choose an `Expect` for the request

    get "https://example.com/api/items/1"
        |> withExpect (Http.expectJson itemsDecoder)
-}
withExpect : Http.Expect b -> RequestBuilder a -> RequestBuilder b
withExpect expect =
    map <| \details -> { details | expect = expect }


{-| Add some query params to the url for the request

    get "https://example.com/api/items/1"
        |> withQueryParams [("hello", "world"), ("foo", "bar")]
        |> withQueryParams [("baz", "qux")]
    -- sends a request to https://example.com/api/items/1?hello=world&foo=bar&baz=qux
-}
withQueryParams : List ( String, String ) -> RequestBuilder a -> RequestBuilder a
withQueryParams queryParams =
    map <| \details -> { details | queryParams = details.queryParams ++ queryParams }


{-| Extract the Http.Request component of the builder, for introspection and
testing
-}
toRequest : RequestBuilder a -> Http.Request a
toRequest (RequestBuilder details) =
    let
        encodedParams =
            joinUrlEncoded details.queryParams

        fullUrl =
            if String.isEmpty encodedParams then
                details.url
            else
                details.url ++ "?" ++ encodedParams
    in
        Http.request
            { method = details.method
            , url = fullUrl
            , headers = details.headers
            , body = details.body
            , expect = details.expect
            , timeout = details.timeout
            , withCredentials = details.withCredentials
            }


{-| Send the request using Http.send
-}
send : (Result Http.Error a -> msg) -> RequestBuilder a -> Cmd msg
send tagger builder =
    Http.send tagger <| toRequest builder


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
