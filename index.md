# Hello Halogen

I need to make a directory for the project so I can begin writing this file. This
means I have to come up with a name for the project, so I think about what it
is: a static blog generator written in Haskell. After a couple of minutes, I
think of "halogen" &mdash; **Ha**skell b**log** G**en**erator. Since this file
represents the blog of the development of Halogen, I'm going to name the repo
something very predicatable:

`mkdir projects/halogen-blog`

`cd projects/halogen-blog`

After many bad experiences with lost files, I always create a git repository
before doing anything, even if I never intend to push it remotely..

`git init .`

Now I can save this file. I'll just name it `index.md` for now, pending
further improvements.

So what is the purpose of Halogen? I am learning Haskell, so I want to write
something that has an actual, real-world use. And speaking of real-world, I just
finished the "I/O" chapter of "Real World Haskell," so I want to practice my
chops. I also want to share my experience of learning Haskell in the hope that
my learning process may be helpful to others. I also want to be able to look
back and learn from my experience later.

Here's what I'm thinking for the MVP of Halogen:

* There will be a single, command-line program named `halogen` which reads files
out of a `src` directory, processes them as Markdown, and places the results in a
`site` directory
* The files in the `src` directory will be the blog posts
* The files in the `site` directory will represent the site, and it can be
served by your web server of choice. I will be using GitHub Pages.
* A post filename is the path in the url that will link to the generated post.
For example, if the filename is "example-post.md," the url to the post would
be "mysite.com/posts/example-post.html."
* A post must contain a JSON metadata header which provides at least the
*title* and *publish_at* timestamps of the post. A basic post file looks like this:

        {
          "title" : "Hello, world!",
          "publish_at" : "2015-06-28T19:07:00Z"
        }

        Hello, world!

* The `halogen` command will also compile an `index.html` file in the `site`
directory that simply links to any post that has a publish timestamp earlier than the
time the site was generated.

It seems like a good time to check in this file.
[462615cea5b5e971103d3d22a67fe8bd31789603]()

***

I know I'm going to need to use cabal, and that I have a couple of dependencies
based on the requirements: a Markdown processor for converting to HTML and a
JSON library for reading and using post metadata. A couple of quick Google
searches point me at [markdown](https://hackage.haskell.org/package/markdown)
and [json](https://hackage.haskell.org/package/json). These seem like
reasonable choices even though a couple of other results come up that seem
interesting. I don't have any reason to choose anything but the bare basic
functionality, so I'm going to go with these until I have a better reason to
use one of the other libraries.

So now I need to remember how to set up a cabal project and get my dependencies
installed. I know enough that I can use cabal to do this, so let's try it out:

    $ cabal install markdown
    Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal
    update' to download it.
    cabal: There is no package named 'markdown'.
    You may need to run 'cabal update' to get the latest list of available
    packages.

Ok, well that's super useful:

    $ cabal update
    Downloading the latest package list from hackage.haskell.org

Cool. That seems like what I'm used to in package managers. Let's try again:

    $ cabal install markdown
    ...crazy pile of output indicating that the command worked...
    Registering markdown-0.1.13.2...
    Installed markdown-0.1.13.2

Super cool. Now I want to try one of the examples in the markdown package
documentation, so I fire up `ghci`:

    $ ghci
    GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Prelude> :set -XOverloadedStrings
    Prelude> import Text.Blaze.Html.Renderer.Text
    Prelude Text.Blaze.Html.Renderer.Text> renderHtml $ markdown def "# Hello World!"

    <interactive>:4:14: Not in scope: `markdown'

    <interactive>:4:23: Not in scope: `def'
    Prelude Text.Blaze.Html.Renderer.Text>

Ok, so I probably need to import something else, maybe `Text.Markdown`:

    Prelude Text.Blaze.Html.Renderer.Text> :m Text.Markdown
    Prelude Text.Markdown> renderHtml $ markdown def "# Hello World!"

    <interactive>:6:1: Not in scope: `renderHtml'
    Prelude Text.Markdown>

Better, but it looks like I'll need to figure out what to import to get
`renderHtml` into scope. Googling "haskell renderHtml" takes me directly to
"Text.Blaze", which seems to make sense, so let's try it:

    :Prelude Text.Markdown> :m Text.Blaze
    Prelude Text.Blaze> renderHtml $ markdown def "# Hello World!"

    <interactive>:8:1: Not in scope: `renderHtml'

    <interactive>:8:14: Not in scope: `markdown'

    <interactive>:8:23: Not in scope: `def'm Text.Blaze

Ah. Now `markdown` isn't in scope anymore, so it looks like the `:m` command in
ghci totally replaces the currently imported modules which the specified one.
Good to know. Let's use a series of `import`s instead:

    Prelude Text.Blaze.Html.Renderer.Text> import Text.Markdown
    Prelude Text.Blaze.Html.Renderer.Text Text.Markdown> renderHtml $ markdown def "# Hello World!"
    ...lots of "Loading package" statements...
    "<h1>Hello World!</h1>"

Awesome! And it's cool to see Haskell's laziness here, as it doesn't load any
of the markdown package's dependencies until they're actually needed. Neat!

So the markdown package is with us, now let's move on to json.

    $ cabal install json
    ...lots of output...
    Registering json-0.9.1...
    Installed json-0.9.1

Works as expected. Looking into the package documentation, I see that there is
a `decode` function that takes a `String` and returns a `Result a`, where `a`
is in the `JSON` typclass. That seems like a good place to start:

    $ ghci
    Prelude> import Text.JSON
    Prelude Text.JSON> let json = "{ \"foo\" : \"bar\", \"baz\" : \"qux\" }"
    Prelude Text.JSON> decode json

    <interactive>:8:1:
        No instance for (JSON a0) arising from a use of `decode'
        The type variable `a0' is ambiguous
        Possible fix: add a type signature that fixes these type variable(s)
        Note: there are several potential instances:
          instance JSON () -- Defined in `Text.JSON'
          instance (JSON a, JSON b) => JSON (a, b) -- Defined in `Text.JSON'
          instance (JSON a, JSON b, JSON c) => JSON (a, b, c)
            -- Defined in `Text.JSON'
          ...plus 31 others
        In the expression: decode json
        In an equation for `it': it = decode json

That doesn't look good, but trying to dicipher the output leads me to the
conclusion that Haskell needs some help figuring out what type `a` is supposed
to be here, so looking into the docs a bit more, I see that there is a
`JSValue` data type. Maybe I can use that:

    Prelude Text.JSON> decode json :: JSValue

    <interactive>:10:1:
        Couldn't match expected type `JSValue' with actual type `Result a0'
        In the return type of a call of `decode'
        In the expression: decode json :: JSValue
        In an equation for `it': it = decode json :: JSValue

Nope. But I'm getting somewhere. `Result` wraps a type, so:

    Prelude Text.JSON> decode json :: Result JSValue
    Ok (JSObject (JSONObject {fromJSObject = [("foo",JSString (JSONString {fromJSString = "bar"})),("baz",JSString (JSONString {fromJSString = "qux"}))]}))

Cool!! That looks like some parsed JSON! I have no idea if this is the _right_
way to do things, but if all else fails, this will work.

I'm ready to start writing some code now, but it's becoming clear that I'll need to make sure that the halogen-blog is actually setup with the appropriate files and directories. Let's first check in these changes, then create some structure and do some renaming:

    git add index.md
    git commit -m "trying out the markdown and json packages"

***


