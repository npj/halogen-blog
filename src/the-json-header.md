{
  "title" : "The JSON Header",
  "publish_at" : "2015-07-06T19:33:00Z"
}

# The JSON Header

The goal is now to decode some JSON directly into a `Header` type. In doing so,
I want the timestamp to be parsed into a `ZonedTime` value in the current time
zone. First I have to create the actual repository.

    > mkdir halogen
    > cd halogen
    > git init .

Here's the first stab at a data type:

    data Header = Header {
      getTitle :: String,
      getPublishAt :: String
    } deriving (Show)

I'm not going to try to get the `publish_at` field into a `ZonedTime` just yet.
The above loads just fine in ghci, so I'll continue with an instance definition
for FromJSON based on the `aeson` documentation (plus a few extra imports):

[halogen: 20a049ce0fa88bf189dc647f6150168bdbf0413b](https://github.com/npj/halogen/commit/20a049ce0fa88bf189dc647f6150168bdbf0413b)

    {-# LANGUAGE OverloadedStrings #-}

    import Data.Aeson
    import Control.Applicative
    import Control.Monad

    data Header = Header {
      getTitle :: String,
      getPublishAt :: String
    } deriving (Show)

    instance FromJSON Header where
      parseJSON (Object json) = Header <$>
                                json .: "title" <*>
                                json .: "publish_at"

      parseJSON _ = mzero

[halogen-blog: 071e2f56af662b820ba052fe3dd5ce1e6853cf65](https://github.com/npj/halogen-blog/commit/071e2f56af662b820ba052fe3dd5ce1e6853cf65)

    > let json = Data.ByteString.Lazy.Char8.pack "{ \"title\" : \"Testing, Testing\", \"publish_at\" : \"2015-06-28T19:03:00Z\" }"
    > decode json :: Maybe Header
    Just (Header {getTitle = "Testing, Testing", getPublishAt = "2015-06-28T19:03:00Z"})

Excellent. Now the trick is to get the `publish_at` field to decode into a
`ZonedTime`. I notice a few things while trying some ideas out
in ghci:

    ghci> let zoned = decode "[\"2015-06-28T19:03:00Z\"]" :: Maybe [ZonedTime]
    ghci> zoned
    Just [2015-06-28 19:03:00 +0000]

Decoding directly into a `ZonedTime` gives me a time with a +0000 zone. The end
result for the user is that the `publish_at` time is treated as a time in their
current time zone. `ZonedTime` is essentially a pairing of a `LocalTime` and a
`TimeZone`. The trick is then to pull out the `LocalTime` and convert it into a
UTC time as if it were in a given `TimeZone` to begin with. Luckily, Haskell
provides a function that does exactly what I want: `localTimeToUTC`.

    ghci> :t localTimeToUTC
    localTimeToUTC :: TimeZone -> LocalTime -> UTCTime

The `zoned` variable is a `ZonedTime` inside a `Just` inside a `[]` so in order
to operate on it, I need to lift a function into the `Maybe` monad. I also need
to extract the current time zone out of `IO`. The function that pulls the local
time out of `ZonedTime` is `zonedTimeToLocalTime`.

    ghci> currentZone <- getCurrentTimeZone
    ghci> currentZone
    PDT
    ghci> liftM (localTimeToUTC currentZone . zonedTimeToLocalTime . head) zoned
    Just 2015-06-29 02:03:00 UTC

Excellent. This is the correct calculation: 7PM Pacific is 2AM UTC. Now it's
time to write a nicer `decodeHeader` function that does all of this work.

First, I'll change the type of `getPublishAt` to `ZonedTime` so `aeson` can
take care of the decoding the date.

[halogen: 335ee233caea4ee8c1586d288a341dc6a711e113](http://github.com/npj/halogen/commit/335ee233caea4ee8c1586d288a341dc6a711e113)

    {-# LANGUAGE OverloadedStrings #-}

    import Data.Aeson
    import Control.Applicative
    import Control.Monad
    import Data.Time.LocalTime

    data Header = Header {
      getTitle :: String,
      getPublishAt :: ZonedTime
    } deriving (Show)

    instance FromJSON Header where
      parseJSON (Object json) = Header <$>
                                json .: "title" <*>
                                json .: "publish_at"

      parseJSON _ = mzero

Trying it out (notice the 'Z' in the timestamp is no longer necessary):

    ghci> let json = Data.ByteString.Lazy.Char8.pack "{ \"title\" : \"Test Post\", \"publish_at\" : \"2015-07-06T19:03:00\" }"
    ghci> decode json :: Maybe Header
    Just (Header {getTitle = "Test Post", getPublishAt = 2015-07-06 19:03:00 +0000})

Now, I'll add a function that takes a JSON `ByteString` and a `TimeZone` and
returns a `Maybe Header` with the `getPublishAt` field calculated in UTC
releative to the `TimeZone`.

[halogen: 45629c7d9d206d01d022e89f361c4c4bee60b911](https://github.com/npj/halogen/commit/45629c7d9d206d01d022e89f361c4c4bee60b911)

    modifyPublishAt :: Header -> ZonedTime -> Header
    modifyPublishAt header time = header { getPublishAt = time }

`modifyPublishAt` is fairly straightforward. It takes a `Header` and a
`ZonedTime` and returns a new `Header` with its `getPublishAt` field set to the
provided `ZonedTime`. The actual complexity comes next:

[halogen: 45629c7d9d206d01d022e89f361c4c4bee60b911](https://github.com/npj/halogen/commit/45629c7d9d206d01d022e89f361c4c4bee60b911)

    decodeHeader :: ByteString -> TimeZone -> Maybe Header
    decodeHeader json tz = toUTC <$> decode json
      where toUTC header = modifyPublishAt header $ convert $ getPublishAt header
            convert = utcToZonedTime utc . localTimeToUTC tz . zonedTimeToLocalTime

Note that I need to add `Data.ByteString.Lazy` to the imports.

`decodeHeader` decodes the incoming `json` `ByteString`, and then maps `toUTC`
across that (`<$>` is just a synonym for `fmap` that looks nice as an infix
operator in this particular case). Since `decode json` returns a `Maybe
Header`, this allows the `toUTC` function to accept an unwraped `Header` which
will then be wrapped back inside a `Just` as a result of the `fmap`. This also
prevents me from having to deal directly with the case of `decode` returning
`Nothing`. Note that I don't have to tell the `decode` function that it returns
a `Maybe Header` because the compiler can infer the return type from the rest
of the function body.

`toUTC` pulls out the decoded "publish_at" value from the supplied `Header`,
passes the result to `convert`, and constructs a new `Header` with a
`getPublishAt` field set to the result of `convert`, via `modifyPublishAt`.

The real mathematical action is in `convert`. It takes a `ZonedTime` and
converts it to a `LocalTime`. It then converts that to a `UTCTime`,
interpreting the `LocalTime` as if it were in the supplied `TimeZone`, which is
the second argument to `decodeHeader`. Then it wraps that into a new
`ZonedTime`, setting the zone directly to `utc`.

Since I will later need to use the impure `getCurrentTimeZone` to get the
user's current time zone, this seems like a good way to parse some json into a
`Header` in a given time zone. If the `Header` type later supports more date
fields, I'll have to come back and modify this function to convert those as
well.

However, I can try this out in ghci now:

    ghci> :set -XOverloadedStrings
    ghci> :load header
    ghci> let json = Data.ByteString.Lazy.Char8.pack "{ \"title\" : \"Test Post\", \"publish_at\" : \"2015-07-06T19:03:00\" }"
    ghci> liftM (decodeHeader json) getCurrentTimeZone
    Just (Header {getTitle = "Test Post", getPublishAt = 2015-07-07 02:03:00 UTC})

It works! And I got to use `liftM` again!

Now to verify that invalid keys return a `Nothing`:

    ghci> let badJson = Data.ByteString.Lazy.Char8.pack "{ \"tortle\" : \"Test Post\", \"publish_at\" : \"2015-07-06T19:03:00\" }"
    ghci> liftM (decodeHeader badJson) getCurrentTimeZone
    Nothing
    ghci> let badIntJson = Data.ByteString.Lazy.Char8.pack "{ \"title\" : 12345, \"publish_at\" : \"2015-07-06T19:03:00\" }"
    ghci> liftM (decodeHeader badIntJson) getCurrentTimeZone
    Nothing

Yay! Using a new type in this way means I don't have to manually check the
structure of the JSON, which is something with which I am all too familiar in
dynamically typed languages.

[halogen-blog: 1e25f3771dcb53c4301625cba12e4020340c3680](https://github.com/npj/halogen-blog/commit/1e25f3771dcb53c4301625cba12e4020340c3680)

