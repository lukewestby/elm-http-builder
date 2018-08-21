# elm-http-builder

Chainable functions for building HTTP requests

**Need help? Join the #http-builder channel in the [Elm Slack](https://elmlang.herokuapp.com)!**

```elm
import Http
import HttpBuilder exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode


itemsDecoder : Decode.Decoder (List String)
itemsDecoder =
    Decode.list Decode.string


itemEncoder : String -> Encode.Value
itemEncoder item =
    Encode.object
        [ ("item", Encode.string item) ]


handleRequestComplete : Result Http.Error (List String) -> Msg
handleRequestComplete result =
    -- Handle the result

{-| addItem will send a post request to
`"http://example.com/api/items?hello=world"` with the given JSON body, a
custom header, and cookies included. It'll try to decode with `itemsDecoder`.
-}
addItem : String -> Cmd Msg
addItem item =
    HttpBuilder.post "http://example.com/api/items"
        |> withQueryParams [ ("hello", "world") ]
        |> withHeader "X-My-Header" "Some Header Value"
        |> withJsonBody (itemEncoder item)
        |> withTimeout 10000
        |> withExpectJson itemsDecoder
        |> withCredentials
        |> send handleRequestComplete
```
