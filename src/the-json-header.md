{
  "title" : "The JSON Header",
  "publish_at" : "2015-07-06T19:33:00Z"
}

# The JSON Header

The goal is now to decode some JSON directly into a `Header` type. In doing so,
I want the timestamp to be parsed into a ZonedTime value in the current time
zone. First I have to create the actual repository.

    > mkdir halogen
    > cd halogen
    > git init .

Here's the first stab at a data type:

    data Header = Header {
      getTitle :: String,
      getPublishAt :: String
    } deriving (Show)

I'm not going to try to get the `publish_at` field into a ZonedTime just yet.
The above loads just fine in ghci, so I'll continue with an instance definition
for FromJSON based on the `aeson` documentation (plus a few extra imports):

[halogen: 20a049ce0fa88bf189dc647f6150168bdbf0413b](https://github.com/npj/halogen/commit/20a049ce0fa88bf189dc647f6150168bdbf0413b)
[halogen-blog: ]()




