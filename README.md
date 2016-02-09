# elm-http-extra

Extra helpers for more easily building Http requests that require greater
configuration than what is provided by `elm-http` out of the box.

> Thanks to @fredcy, @rileylark, and @etaque for the original discussion of the API

## Example

```elm
import Time
import Http.Extra as HttpExtra exposing (..)
import Json.Decode as Json


type alias ListStringResponse =
  HttpExtra.Response (List String)


itemsDecoder : Json.Decoder (List String)
itemsDecoder =
  Json.list Json.string


type alias StringError =
  HttpExtra.Error String


errorMessageDecoder : Json.Decoder String
errorMessageDecoder =
  Json.string


addItem : String -> Task StringError ListStringResponse
addItem item =
  HttpExtra.post "http://example.com/api/items"
    |> withBody (Http.string "{ \"item\": \"" ++ item ++ "\" }")
    |> withHeader ("Content-Type", "application/json")
    |> withTimeout (10 * Time.second)
    |> withCredentials
    |> send itemsDecoder errorMessageDecoder
```

## Contributing

I'm happy to receive any feedback and ideas for about additional features. Any input and pull requests are very welcome and encouraged. If you'd like to help or have ideas, get in touch with me at @luke_dot_js on Twitter or @luke in the elmlang Slack!
