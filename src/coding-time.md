{
  "title" : "Coding Time, or So I Thought When I Started Writing",
  "publish_at" : "2015-07-3T11:33:00Z"
}

# Coding Time, or So I Thought When I Started Writing

Now it's time to start writing some actual code, but in thinking about the
program architecture, I realize I've forgotten the JSON header of the previous
post. I'm going to go back and add that now. This also makes me think a little
about error cases. I'm going to have to gracefully handle source posts with no
JSON header, malformed headers, invalid headers, etc...

    git add src/hello-halogen.md
    git commit -m "adds missing JSON header"
    git push

[172318b8eba549e0c20153818b3ecb06d95c9ef5](https://github.com/npj/halogen-blog/commit/172318b8eba549e0c20153818b3ecb06d95c9ef5)

I've always liked to think about handling error cases before focusing on the
general case. Inspired by the myriad possible malformed post possibilities, I
decide to explore the json package a little more in-depth.

    > ghci
    > :m + Text.JSON
    > let json = "{ \"title\" : \"Hello Halogen\", \"publish_at\" : \"2015-06-28T19:07:00Z\" }"
    > let badJson = "{ \"title : \"Hello Halogen\", \"publish_at\" : \"2015-06-28T19:07:00Z\" }"
    > decode json :: Result JSValue
    Ok (JSObject (JSONObject {fromJSObject = [("title",JSString (JSONString {fromJSString = "Hello Halogen"})),("publish_at",JSString (JSONString {fromJSString = "2015-06-28T19:07:00Z"}))]}))
    > decode badJson :: Result JSValue
    Error "Malformed JSON labelled field: Hello Ha"

So the decode function returns a value wrapped in the `Result` type, which is
either `Ok` followed by the data, or `Error` followed by the error message.

[b5963660b3073d2db69948f318e638bb81ea7696](https://github.com/npj/halogen-blog/commit/b5963660b3073d2db69948f318e638bb81ea7696)

This is all well and good for malformed JSON, but what about missing, or
unrecognized fields? Not to mention that the work of actually unwrapping all
this data from the multi-layered types that are present seems like something
that should be "automatic" in Haskell. Ideally, I should be able to parse JSON
directly into a new `Header` type. Looking into the documentation of `Text.JSON`
a little more, I see that there is indeed a JSON typeclass. The task at hand is
to create a new `Header` data type and define a JSON instance for it.

However, being the Haskell noob that I am, I need a lot of help figuring out
exactly how to go about this. Turning to Google almost immediately informs me
that `Text.JSON`  is essentially unused, and that the `aeson` package has become
the de-facto standard for handling JSON in Haskell. It seems pretty clear that
that's the direction in which I want to look.

Investigating the documentation, I find a very satisfactory "How to use this
library." It immediately confirms that I'm on the right track with the idea to
definte a new data type. I want something that has a `title` field with a type
of `String`, and a `publish_at` field with some type that represents a date and
time. The `Data.Aeson` documentation shows that there is a `FromJSON` instance
for `ZonedTime`, so that seems like a reasonable place to start exploring.

The first step is to install the `aeson` package. I'd like also to remove the
`json` package but that seems a bit more complicated, and isn't so important
right now.

    > cabal install aeson
    ...a bunch of stuff...
    > ghci
    ghci> :m +Data.Aeson

Everything looks good, but as soon as I try to do something with `ZonedTime` it
becomes apparent that I have other dependencies. I need the `time` package,
which I assume is a dependency of `aeson`.

    ghci> :m +Data.Time
    ghci> :t ZonedTime
    ZonedTime :: LocalTime -> TimeZone -> ZonedTime

I can turn a `LocalTime` and a `TimeZone` into a `ZonedTime`. What's a
`LocalTime`? Following the docs, it's a `Day` and a `TimeOfDay`, and a
`TimeOfDay` is an `Int`, an `Int`, and a `Pico` representing the hour, minute,
and second respectively. A `Day` on the other hand is more interesting: it's an
`Integer` representing the number of days since 1858-11-17. What could the
significance of that date possibly be? I'll store that question away for now.

The `aeson` docs say that the JSON typeclass has a parseJSON method, so it
seems I should be able to `deocde` some JSON into a `ZonedTime`.

    ghci> decode "[\"2015-06-28T19:03:00\"]" :: Maybe [ZonedTime]

    <interactive>:6:8:
        Couldn't match expected type ‘Data.ByteString.Lazy.Internal.ByteString’
                    with actual type ‘[Char]’
        In the first argument of ‘decode’, namely
          ‘"[\"2015-06-28T19:03:00\"]"’
        In the expression:
            decode "[\"2015-06-28T19:03:00\"]" :: Maybe [ZonedTime]
        In an equation for ‘it’:
            it = decode "[\"2015-06-28T19:03:00\"]" :: Maybe [ZonedTime]

That looks terrifying. After some digging I realize I need to enable the
OverloadStrings extension for my experiment to work.

    ghci> :set -XOverloadedStrings
    ghci> decode "[\"2015-06-28T19:03:00Z\"]" :: Maybe [ZonedTime]
    Just [2015-06-28 19:03:00 +0000]

Fantastic! However, it doesn't give me a time in my time zone. That makes
sense, since my current time zone exists in the real, impure world. I'm going
to have to figure out a different way to get a time in the current time zone. I
know that `getCurrentTimeZone` is going to be of use somehow. I found it
because I knew about `getCurrentTime`, so in ghci I typed `get` and tabbed to
see a list of possible completions, one of which was `getCurrentTimeZone`.

The behavior I'd like is that the `publish_at` time gets parsed in the local
time zone in which the `halogen` command is run. The only reasonable way to do
this is to build it into the `Header` `FromJSON` instance's `parseJSON`
function. It's high time to define this `Header` data type. And to do that, I'm
going to have to start writing some code. And since I'm at the end of this
post, I'm going to rename it: "Coding Time, or So I Thought When I Started
Writing."

[]()
