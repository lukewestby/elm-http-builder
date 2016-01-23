# elm-http-extra

Extra helpers for more easily building Http requests that require greater
configuration than what is provided by `elm-http` out of the box.

## Example

```elm
import Time
import Http
import Http.Extra as HttpExtra
import Json.Decode as Json


itemsDecoder : Json.Decoder (List String)
itemsDecoder =
  Json.list Json.string


addItem : String -> Task Http.Error (List String)
addItem item =
  HttpExtra.post "http://example.com/api/items"
    |> withBody (Http.string "{ \"item\": \"" ++ item ++ "\" }")
    |> withHeader ("Content-Type", "application/json")
    |> withTimeout (10 * Time.second)
    |> withCredentials
    |> send itemsDecoder
```

## Contributing

I'm happy to receive any feedback and ideas for about additional features. Any input and pull requests are very welcome and encouraged. If you'd like to help or have ideas, get in touch with me at @luke_dot_js on Twitter or @luke in the elmlang Slack!
