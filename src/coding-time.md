{
  "title" : "Coding Time",
  "publish_at" : "2015-07-3T11:33:00Z"
}

# Coding Time

Now it's time to start writing some actual code, but in thinking about the
program architecture, I realize I've forgotten the JSON header of the previous
post. I'm going to go back and add that now. This also makes me think a little
about error cases. I'm going to have to gracefully handle source posts with no
JSON header, malformed headers, invalid headers, etc...

    git add src/hello-halogen.md
    git commit -m "adds missing JSON header"
    git push

[172318b8eba549e0c20153818b3ecb06d95c9ef5](https://github.com/npj/halogen-blog/commit/172318b8eba549e0c20153818b3ecb06d95c9ef5)

I've always liked to think about handling error cases before focusing on the general case. Inspired by the myriad possible malformed post possibilities, I decide to explore the json package a little more in-depth.

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
