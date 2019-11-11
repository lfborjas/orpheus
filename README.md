# Orpheus

Poem transcription toolkit, for personal use and to learn more Elm and Haskell.

## 'Sup with that name?

Of course it ultimately is about [Orpheus](https://en.wikipedia.org/wiki/Orpheus), but the choice to use it for this project was more directly inspired by Rainer Maria Rilke's 3rd Sonnet to Orpheus:

```
Ein Gott vermags. Wie aber, sag mir, soll
Mann ihm folgen durch die schmale Leier?
Sinn ist Zwiespalts. An der Kreuzung zweier
Herzwege steht kein Tempel für Apoll.

Gesang, wie du ihn lehrst, ist nicht Begehr,
nicht Werbung um ein endlich noch Erreichtes;
Gesang ist Dasein. Für den Gott ein Leichtes.
Wann aber sind wir? Und wann wendet er

an unser Sein die Erde und die Sterne?
Dies ists nicht, Jüngling, daß du liebst, wenn auch
die Stimme dann den Mund dir aufstößt,—lerne

vergessen, daß du aufsangst. Das verrinnt.
In Wahrheit singen, ist ein andrer Hauch.
Ein Hauch um nichts. Ein Wehn im Gott. Ein Wind.
```

## Backend

### Endpoints

Right now, only one of interest:

    curl -H "Content-Type: application/json" -vd '{"gsURI":  "gs://orpheus-ocr-artifacts/IMG_5013.jpg"}' http://localhost:3030/api/poems/analyze > example.json
    
See the `example-response.json` file for an example.

### Development

Should be up on `localhost:3030` when running `main` from the repl, or when running `stack exec`

### Setup

This project uses `stack`, so make sure that `stack build` works.

As for credentials, you'll need to set up a Google Cloud Platform project, with a publicly accessible bucket, and the Vision API enabled. Follow these instructions:

https://cloud.google.com/vision/docs/ocr

Currently, the `Vision.hs` module simply uses an API Key for authentication, follow the instructions here to generate one: https://cloud.google.com/docs/authentication/api-keys#using_an_api_key

The types in `Vision.hs` should be pretty good to indicate what the raw response from Google looks like, but here's the reference: https://cloud.google.com/vision/docs/reference/rest/v1/AnnotateImageResponse

### Reference

**N.B.** Links are to good tutorials/docs, not necessarily the official libraries.

* [Aeson](https://artyom.me/aeson#), for JSON encoding/decoding
* [Lens](https://github.com/Gabriel439/Haskell-Lens-Tutorial-Library/blob/master/src/Control/Lens/Tutorial.hs), for masochism (and some nifty data access I guess.)
* [Servant](https://www.servant.dev/), for quite strongly typed RESTful API building.
* [Gogol](https://github.com/brendanhay/gogol/blob/develop/examples/src/Example/Storage.hs) for google storage.
* [servant-auth](https://github.com/haskell-servant/servant-auth#readme) for authentication
* [servant-elm](https://github.com/haskell-servant/servant-elm) to generate types to be consumed by the frontend.


## Frontend

### Development

Run `elm-live src/Main.elm` and go to `localhost:8000`. Should reflect changes to code!

### Reference
* [elm-live](https://github.com/wking-io/elm-live)
* [elm-bulma](https://package.elm-lang.org/packages/surprisetalk/elm-bulma/6.1.6)
* [elm-mode](https://github.com/jcollard/elm-mode) for Emacs
