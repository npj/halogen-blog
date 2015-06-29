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
[]()

***
