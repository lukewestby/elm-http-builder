# elm-http-extra

[![ICARE](https://icarebadge.com/ICARE-white.png)](https://icarebadge.com)

Extra helpers for more easily building and handling Http requests that require
greater configuration than what is provided by `elm-http` out of the box.


> Thanks to @fredcy, @rileylark, and @etaque for the original discussion of the
  API

## Example

In this example, we expect a successful response to be JSON array of strings,
like:

```json
["hello", "world", "this", "is", "the", "best", "json", "ever"]
```

and an error response might have a body which just includes text, such as the
following for a 404 error:

```json
Not Found.
```

We'll use `HttpExtra.jsonReader` and a `Json.Decode.Decoder` to parse the
successful response body and `HttpExtra.stringReader` to accept a string
body on error without trying to parse JSON.

```elm
import Time
import Http.Extra as HttpExtra exposing (..)
import Json.Decode as Json


itemsDecoder : Json.Decoder (List String)
itemsDecoder =
  Json.list Json.string


addItem : String -> Task (HttpExtra.Error String) (HttpExtra.Response (List String))
addItem item =
  HttpExtra.post "http://example.com/api/items"
    |> withBody (Http.string "{ \"item\": \"" ++ item ++ "\" }")
    |> withHeader "Content-Type" "application/json"
    |> withTimeout (10 * Time.second)
    |> withCredentials
    |> send (jsonReader itemsDecoder) stringReader
```

## Contributing

 I'm happy to receive any feedback and ideas for about additional features. Any
input and pull requests are very welcome and encouraged. If you'd like to help
or have ideas, get in touch with me at @luke_dot_js on Twitter or @luke in the
elmlang Slack!
