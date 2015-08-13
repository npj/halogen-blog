{
  "title" : "Preprocessing",
  "publish_at" : "2015-07-08T19:29:00"
}

# Preprocessing

The next challenge is going to be extracting the JSON header from a post, and
using the result to decode into a `Header`. This is going to involve parsing
text, so I need some ground rules:

1. The JSON header must be the first thing in a post.
1. The JSON header is a single JSON object, and must be valid.
1. The JSON header can have any amount of leading or trailing whitespace, as
long as the actual text represnting the object is valid JSON.

As such, the parsing rules are fairly straightforward:

* Read the post contents, skipping over any whitespace until a `{` is
encountered.
* Any time a `{` is encountered, read an __object__
* Any time a `"` is encountered, read a __string__
* When a `\` (escape) character is encountered while reading a __string__, collect the next character and prepend it with `\`.
* If a non-whitespace is encountered before the first `{`, the post is invalid.
* If there is an unterminated __object__ or __string__, the post is invalid.

The meaning of "read an object" and "read a string" will become clear later. It's important to remember that I don't want to parse JSON here since that is what `aeson` is for. Rather, I want to extract a string that _could_ be JSON and try to decode that into a `Header`, letting `aeson` do the work of interpreting the JSON.

The general structure of the preprocessing code is to consume part of a string
to return a value, and pass the rest of the string to the next value-consuming
function, and so on until the preprocessing can be considered done. It would be
possible to to this with a series of functions that return tuples, but that
doesn't seem particularly sophisitcated given that *Learn You a Haskell for
Great Good* talks about the `State` monad, which seems like a perfect fit for
this particular problem. The *state* in this case is the input string, and the
value is the extracted string that will subsequently be fed to `decodeHeader`,
or `Nothing` in the event of malformed input.

In order to better understand the `State` monad, I revisted [the section of *Learn You a Haskell for Great Good* on stateful computations](http://learnyouahaskell.com/for-a-few-monads-more#state), and [this particularly illuminating blog post](http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/).

Thinking about extracting the header as a stateful computation means that the problem can be broken down into even more detail.

1. The type of the state is a `ByteString` representing the post data.
1. The value of each stateful computation will be a `Maybe ByteString` which represents either the successfully extracted header, or nothing in the case of a badly formatted post and/or header.
1. At the end of the computation, the value holds the extracted header JSON and the state holds the remainder of the post.

Just as with many parsing strategies, the code will read a character at a time, and for each character make a decision about the next thing to do. As such, a good place to start would be to make a stateful `readChar` function. This function should read the head from the state value and return it along with the the tail of the original state string.


	import Control.Monad.State (State, get, put)
	import Data.ByteString.Lazy.Char8 (ByteString, uncons)

	readChar :: State ByteString (Maybe Char)
	readChar = do
	  input <- get
	  case (uncons input) of
	    Just (c,rest) -> do
	      put rest
	      return $ Just c
	    Nothing -> return Nothing


Note that I'm explicitly listing the types and functions to import from each module. I think this is generally good practice as it reduces ambiguity and serves as a kind of documentation as well.

So back to the code: the first weird thing about this is that the function takes no arguments. According to the type signature, it simply returns a `State ByteString (Maybe Char)`. This is fundamental to how the `State` monad works. Rather than writing functions which take values and return values, one writes functions that return a function which will eventually operate on a state and produce a value. Working with the `State` monad is like building up a bunch of little machines that chain onto one another. The entire machine isn't set into motion until it is fed an initial state via `runState`.

This will make more sense as more parsing functions come into play. For now, there is only one machine called `readChar`, and it can already be taken for a test drive in GHCI. Since I need to use `runState`, I'll have to add it to the list of functions imported from `Control.Monad.State`:


	import Control.Monad.State (State, get, put, runState)


Now I can load `header.hs` into GHCI:

	ghci> :load header
	ghci> let input = Data.ByteString.Lazy.Char8.pack "this is a test"
	ghci> input
	"this is a test"
	ghci> runState readChar input
	(Just 't',"his is a test")

Notice that the `fst` of the tuple is `Just 't'`. Here's what happens when the same code is run with an empty input string:

	ghci> let input = Data.ByteString.Laxy.Char8.empty
	ghci> input
	""
	ghci> runState readChar input
	(Nothing,"")

The possibility of failure is baked into the parsing code. Functions that build off of `readChar` can react to what it returns when deciding what to do next. 

## Reading Strings

Now something more complex can be built. When a `"` character is encountered, everything until the next `"` must be combined into a new string, but any character preceded by a `\` must be unconditionally included as well. For example, both of the following are valid strings:

* `"hello, this is a string"`
* `"hello, \"this\" is a string\nand this: it\'s a quotation mark: \""`

The `readString` function has a similar type signature, but it returns a `Maybe  ByteString` instead of a `Maybe Char`. The concept is straightforward: read a character and decide what to do next based on that. The only trick is that this is a co-routine, which will get run by a higher-level function as a result of reading an initial `"`. As such, `readString` reads everything until it encounters a `"`.


	import Data.ByteString.Lazy.Char8 (ByteString, cons, pack, uncons)

	readString :: State ByteString (Maybe ByteString)
	readString = do
	  char <- readChar
	  case char of
        Just '"' -> return $ Just $ pack "\""
    	 
	    Just '\\' -> do
	      next <- readChar
	      case next of
	        Just c -> do
	          rest <- readString
	          case rest of
	            Just r -> return $ Just (cons '\\' (cons c r))
	            Nothing -> return Nothing
	        Nothing -> return Nothing

	    Just c -> do
	      rest <- readString
	      case rest of
	        Just r -> return $ Just (cons c r)
	        Nothing -> return Nothing

	    Nothing -> return Nothing

Look at all those `Just`s and `Nothing`s! It's easy enough to follow what this function does, but it's pretty hideous. Every time the `<-` operator unwraps something from the `State` monad, it's still sitting inside a `Maybe`, so the code needs to check whether the result is `Just` or `Nothing`. Furthermore, the code always needs to re-wrap the results in a `Maybe`, then `return` that, which re-wraps it in the `State` monad. But, it works:

	ghci> runState readString (pack "hello\"")
	(Just "hello\"","")
	ghci> runState readString (pack "what\'s up?\"")
	(Just "what's up?\"","")
	ghci> runState readString (pack "unclosed string")
	(Nothing,"")

All this unwrapping and rewrapping sounds like a common problem. This is why the `liftM` family of functions exists. These functions take a function that operate on "naked" values and allow them to work inside values that are wrapped. For example:

	ghci> liftM (+5) (Just 2)
	Just 7
	ghci> liftM2 (+) (Just 5) (Just 2)
	Just 7
	ghci> liftM2 (+) (Just 5) Nothing
	Nothing

In order to make `readString` a little less hard on the eyes, I'll need to make use of these lifting functions. When I refactor code, I like to write the code I want first, then make that code work later. The first candidate for improvement is the code above that prepends a regular character to the rest of the parsed string:

	Just c -> do
      rest <- readString
      case rest of
        Just r -> return $ Just (cons c r)
        Nothing -> return Nothing

It'd be nice if I could do something like this:

	Just c -> c : readString

That's certainly not going to work because `:` has a type signature of `a -> [a] -> [a]`, and here `c` is a `Char` and `readString` is a `State ByteString (Maybe ByteString)`. But, I can write a _new_ operator that operates on values of these types. It will need to do some lifting, so I'll call it `^:` since it's like "lift `:`."

	(^:) :: Char -> State ByteString (Maybe ByteString) -> State ByteString (Maybe ByteString)
    c ^: str = liftM (liftM (cons c)) str

The innermost `liftM` lifts the function `cons c` into `Maybe`. (Remember that `str` is a `ByteString`, not a `String`, so the normal `:` operator won't work. The analog for `ByteString` is `cons`.) Then, the result of lifting `cons c` into `Maybe` is lifted into `State ByteString (Maybe ByteString)` and applied to `str`. This allows the section above to be re-written as:

  `Just c -> c ^: readString`

That's pretty close to the code I wanted. The entirety of `readString` now looks like this:

    readString :: State ByteString (Maybe ByteString)
    readString = do
      char <- readChar
      case char of
        Just '"' -> return $ Just $ pack "\""
        
        Just '\\' -> do
          next <- readChar
          case next of
            Just c -> do
              rest <- readString
              case rest of
                Just r -> return $ Just (cons '\\' (cons c r))
                Nothing -> return Nothing
            Nothing -> return Nothing

        Just c -> c ^: readString

        Nothing -> return Nothing

Try it out with the same inputs as before, but don't forget to `import Control.Monad (liftM)`. From this point forward I won't continue to write the new imports needed, as it's a fairly straightforward process to determine what's missing and add it.

Armed with the success of this new operator, it's time to tackle the next bit of uggo code:

    Just '\\' -> do
      next <- readChar
      case next of
        Just c -> do
          rest <- readString
          case rest of
            Just r -> return $ Just (cons '\\' (cons c r))
            Nothing -> return Nothing
        Nothing -> return Nothing

Using the same process as before, I'd like to write what I want the code to be and implement that later. All that's happening here is that a `\` character has been read, meaning the next character is escaped and should be included in the final result unconditionally. So the next character after that is read, and as long as it's `Just` something, the rest of the string is read, and as long as _that_ is `Just ` something, everything gets glued together and re-wrapped with `Just` and `return`. If the types in play were all regular `String`s and `Char`s, the code would look something like this:

    Just '\\' -> '\\':(readChar:readString)
    
This obviously won't work since everything is a `Maybe` wrapped in a `State`. The leftmost `:` above will need to be the new `^:` operator.

	Just '\\' -> '\\' ^: (readChar:readString)
	
But, the next `:` needs to bea new operator, since it has to `cons` a `State ByteString (Maybe Char)` with a `State ByteString (Maybe ByteString)`. Since the new operator will be a monadic `:`, it seems fitting to call it `>>:`. But before I implement that, I want to do something about these type signatures. They are getting pretty long. So far there are a few things that operate on `State ByteString (Maybe Char)` and `State ByteString (Maybe ByteString)`, so I can make a type synonnym:

	type Parse a = State ByteString (Maybe a)
	
Now every `State ByteString (Maybe Char)` becomes `Parse Char` and every `State ByteString (Maybe ByteString)` becomes `Parse ByteString`. The type signatures for everything seen so far are now:

	(^:) :: Char -> Parse ByteString -> Parse ByteString
	readChar :: Parse Char
	readString :: Parse ByteString
	
Not only are the type signatures shorter, but to me they are also much more readable.

The type signature for `>>:` will be:

	(>>:) :: Parse Char -> Parse ByteString -> Parse ByteString
	
The implementation is fairly similar to `^:`, but since all of the arguments are already wrapped in `Parse`, it's enough to lift `cons` into `Maybe`, and then again into `State`. And since `cons` is binary, `liftM2` is needed:

	(>>:) = liftM2 $ liftM2 cons
	
That's it! Feeding this back into `readString` yields a greatly simplified implementation:

	readString :: Parse ByteString
	readString = do
	  char <- readChar
	  case char of
		Just '"'  -> return $ Just $ pack "\""
	    Just '\\' -> '\\' ^: (readChar >>: readString)
	    Just c    -> c ^: readString
	    Nothing   -> return Nothing

In fact, now that there is `>>:`, `^:` can be defined in terms of it to make things even simpler. To do this, a regular `Char` must be converted into a `Parse Char` to be passed as the first argument to `>>:`. That's easy with `return` and the `Just` type constructor:

	c ^: str = (return $ Just c) >>: str
	
That's about as straightforward as I'm able to make `readString`.

## Reading Objects

Reading an object boils down to reading everything between a `{` and a `}`, being aware not to prematurely stop reading if an escaped `}` is encountered while reading a string. That logic is already taken care of by `readString`, so `readObject` just needs to delegate to `readString` when it encounters a `"`. Remember that the assumption is that a higher-level function has already read a `{` and is calling `readObject` as a result, so `readObject` needs to read everything up until the terminating `}`:

	readObject :: Parse ByteString
	readObject = do
	  char <- readChar
	  case char of
	    Just '}' -> return $ Just $ pack "}"

		Just '"' -> do
	      str <- readString
	      case str of
	        Just s -> do
	          rest <- readObject
	          case rest of
	            Just r -> return $ Just $ cons '"' $ append s r
	            Nothing -> return Nothing
	        Nothing -> return Nothing
	        
	    Just '{' -> do
	      sub <- readObject
	      case sub of
	        Just o -> do
	          rest <- readObject
	          case rest of
	            Just r -> return $ Just $ cons '{' $ append o r
	            Nothing -> return Nothing
	        Nothing -> return Nothing

	    Just c -> c ^: readObject

	    Nothing -> return Nothing

It's clear that this implementation has some of the same issues as the original `readString`, but it gets the job done:

	ghci> let input = pack " \"greeting\" : \"hello\", \"responses\" : { \"brief\" : \"hey\", \"cordial\" : \"how do you do\", \"angry\" : \"...\" } }"
	ghci> runState readObject input
	(Just " \"greeting\" : \"hello\", \"responses\" : { \"brief\" : \"hey\", \"cordial\" : \"how do you do\", \"angry\" : \"...\" } }","")
	ghci> let input = pack " \"greeting : \"hello\", \"responses\" : { \"brief\" : \"hey\", \"cordial\" : \"how do you do\", \"angry\" : \"...\" } }"
	ghci> runState readObject input
	(Nothing,"")

Just like `readString`, `readObject` can be improved greatly with a new operator. Looking at the branch of the `case` expression which handles reading a string, it's clear that the end result of all the `Just` and `Nothing` checks is to read the substring and append the rest of the object currently being read:

    Just '"' -> do
      str <- readString
      case str of
        Just s -> do
          rest <- readObject
          case rest of
            Just r -> return $ Just $ cons '"' $ append s r
            Nothing -> return Nothing
        Nothing -> return Nothing

This could be simplified as:

	Just '"' -> '"' ^: (readString ++ readObject)
	
It's clear that `++` isn't going to work since `readString` and `readObject` are both `Parse ByteString`, so I'll need a new operator that lifts `append`. Since it will be a monadic analog to `++`, prefixing as before seems fitting: `>>++`. The implementation almost identical to `>>:`, but `append` is lifted instead of `cons`:

	(>>++) :: Parse ByteString -> Parse ByteString -> Parse ByteString
	(>>++) = liftM2 $ liftM2 append
	
The `readObject` implementation can now be simplified to:

	readObject :: Parse ByteString
	readObject = do
	  char <- readChar
	  case char of
	    Just '}' -> return $ Just $ pack "}"

	    Just '"' -> '"' ^: (readString >>++ readObject)

	    Just '{' -> do
	      sub <- readObject
	      case sub of
	        Just o -> do
	          rest <- readObject
	          case rest of
	            Just r -> return $ Just $ cons '{' $ append o r
	            Nothing -> return Nothing
	        Nothing -> return Nothing

	    Just c -> c ^: readObject

	    Nothing -> return Nothing

The branch which handles reading a sub-object naturally reduces using this new operator as well, which yields the final `readObject`:

	readObject :: Parse ByteString
	readObject = do
	  char <- readChar
	  case char of
	    Just '}' -> return $ Just $ pack "}"
	    Just '"' -> '"' ^: (readString >>++ readObject)
	    Just '{' -> '{' ^: (readObject >>++ readObject)
	    Just c   -> c ^: readObject
	    Nothing  -> return Nothing

## Tying It All Together

The last thing to do is write a `headerExtractor` function which will be the highest-level parsing function. It's job is to take a `ByteString` representing a post, strip away all the initial whitespace, read a single json object with the `readObject` function, and strip away any remaining whitespace after. It's a failrly straightforward implementation:

	headerExtractor :: Parse ByteString
	headerExtractor = do
	  skipSpace
	  char <- readChar
	  case char of
	    Just '{' -> do
	      header <- '{' ^: readObject
	      skipSpace
	      return header
	    Nothing -> return Nothing

The implementation of `skipSpace` is also very simple, but since it uses `dropWhile`, ghc doesn't know whether to use the one from `Prelude` or the imported `ByteString` functions. This can be solved by hiding `dropWhile` from `Prelude` with the following:

	import Prelude hiding (dropWhile)
	
Then, the implementation of `skipSpace` will compile without issues:

	skipSpace :: Parse ()
	skipSpace = do
	  input <- get
	  put $ dropWhile isSpace input
	  return Nothing

It simply gets the `ByteString` that represents the state, and replaces the state with a new `ByteString` that has no leading spaces. Now that the `headerExtractor` exists, the final `extractHeader` function can be written. It takes a `ByteString` representing a post, and returns a tuple where the first element is a `Maybe ByteString` representing the text of the header, and the second element is the remaining text of the post. Later on, the header text will be passed to `decodeHeader`, and the remaining post text will be run through a markdown converter.

The `extractHeader` function is trivial:

	extractHeader :: ByteString -> (Maybe ByteString, ByteString)
	extractHeader = runState headerExtractor


 


