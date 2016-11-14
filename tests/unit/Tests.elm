module Tests exposing (..)

import Expect
import Time
import Test exposing (..)
import Http
import HttpBuilder exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Native.Polyfills


polyfillsEnabled : Bool
polyfillsEnabled =
    Native.Polyfills.enabled


toTuple : RequestBuilder -> ( Http.Request (Response String), Settings )
toTuple builder =
    ( toRequest builder stringReader, toSettings builder )


testDecoder : Decode.Decoder String
testDecoder =
    Decode.at [ "hello" ] Decode.string


initialRequest : Request String
initialRequest =
    { method = "GET"
    , url = "http://example.com"
    , headers = []
    , body = Http.emptyBody
    , expect = Http.expectString
    }


all : Test
all =
    describe "All tests"
        [ test "polyfills are set up" <| \() -> Expect.equal polyfillsEnabled True
        , describe "Request Building"
            [ test "should build request and settings with expected params" <|
                \() ->
                    get "http://example.com"
                        |> withHeader "Test" "Header"
                        |> withHeaders [ ( "OtherTest", "Header" ) ]
                        |> withStringBody "" """{ "test": "body" }"""
                        |> withTimeout (10 * Time.second)
                        |> withCredentials
                        |> toRequestRecord stringReader
                        |> (\r -> { r | expect = Http.expectString })
                        |> Expect.equal
                            { method = "GET"
                            , url = "http://example.com"
                            , headers = List.map (uncurry Http.header) [ ( "OtherTest", "Header" ), ( "Test", "Header" ) ]
                            , body = Http.stringBody "" """{ "test": "body" }"""
                            , expect = Http.expectString
                            , timeout = Just (10 * Time.second)
                            , withCredentials = True
                            }
            ]
        , describe "with*Body functions"
            [ test "withStringBody applies string directly" <|
                \() ->
                    get "http://example.com"
                        |> withStringBody "" "hello"
                        |> toRequestRecord stringReader
                        |> .body
                        |> Expect.equal (Http.stringBody "" "hello")
            , test "withJsonBody applies a Json.Value as a string" <|
                \() ->
                    get "http://example.com"
                        |> withJsonBody (Encode.string "hello")
                        |> toRequestRecord stringReader
                        |> .body
                        |> Expect.equal (Http.stringBody "application/json" "\"hello\"")
            , test "withUrlEncodedBody encodes pairs as url components" <|
                \() ->
                    get "http://example.com"
                        |> withUrlEncodedBody [ ( "hello", "world" ), ( "test", "stuff" ) ]
                        |> toRequestRecord stringReader
                        |> .body
                        |> Expect.equal (Http.stringBody "application/x-www-form-urlencoded" "hello=world&test=stuff")
            , test "withMultipartBody passes through to Http.multipart" <|
                \() ->
                    get "http://example.com"
                        |> withMultipartBody [ Http.stringPart "hello" "world" ]
                        |> toRequestRecord stringReader
                        |> .body
                        |> Expect.equal (Http.multipartBody [ Http.stringPart "hello" "world" ])
            , test "withMultipartStringBody first converts string pairs to string data and then passes to multipart" <|
                \() ->
                    get "http://example.com"
                        |> withMultipartStringBody [ ( "hello", "world" ) ]
                        |> toRequestRecord stringReader
                        |> .body
                        |> Expect.equal (Http.multipartBody [ Http.stringPart "hello" "world" ])
            ]
        , describe "BodyReaders"
            [ test "should extract a text value as a string" <|
                \() ->
                    stringReader ("test value")
                        |> Expect.equal (Ok "test value")
            , test "should extract a json value with a decoder" <|
                \() ->
                    jsonReader testDecoder ("""{ "hello": "world" }""")
                        |> Expect.equal (Ok "world")
            ]
        ]
