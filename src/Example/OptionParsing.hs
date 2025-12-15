-- PROBLEM: My application needs a familiar and consistent command line interface.
-- SOLUTIONS: (1) Use one of the purpose built libraries for this.
module Example.OptionParsing where

import Control.Monad ((>=>))
import Data.Maybe (maybe)
import Options.Applicative
import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.Environment
import System.Exit

{-
Command line arguments look like this (the part after `foo`):

    foo --option1=value1 --option2=value2 ... --optionN=valueN

And they generally follow some conventions: the order of the options does not matter, and sometimes it is desireable to allow the same option name to appear more than once. There's some additional structure--most tools follow a standardized format for their usage instructions and man pages, which must be kept in sync with the code.

This is a perfect example of a problem to be solved with a library. Nearly everyone needs to solve this problem, there's a lot of fiddly details to get right, and there is a significant benefit for users if everyone just uses the same thing (principle of least astonishment).
-}

{-
I know of two approaches: using `System.Console.GetOpt` or the `optparse-applicative` library. This module implements the same interface using both in order to compare. The example app should take the following arguments:

    * '--help' or '-?', which if present causes the app to print usage instructions and exit.
    * '--input', which if present has a required argument 'filename' which is the path to a
      readable file. If this file is not readable the app should exit gracefully and explain why.
      If the option is not present, the default is `stdin`. In either case the app should
      write the input to stdout, replacing any occurrences of the suffix "-am" by "-amalamadingdong"
    * '--shout', which takes an optional argument. If the argument is present, then all occurrences
      of that string should be capitalized in the output. If the argument is not present then the
      entire output should be capitalized. This option can appear multiple times.

To run:
    cabal run haskell-examples:option-parsing -- #args go here
-}

optionParsingMain :: IO ()
optionParsingMain = do
  putStrLn
    "This program will parse its command line arguments using one of a few \
    \different approaches. Which one is determined by the value of the \
    \OPTION_PARSE_VARIANT environment variable.\n"
  putStrLn
    "Select an approach using the following values for that variable:\n\
    \  * 'getopt-data'    : System.Console.GetOpt with a flag bag (`a`)\n\
    \  * 'getopt-func'    : System.Console.GetOpt with functions (`a -> a`)\n\
    \  * 'getopt-kleisli' : System.Console.GetOpt with kleisli arrows (`a -> m a`)\n"
  var <- getEnv "OPTION_PARSE_VARIANT"
  case var of
    "getopt-data"          -> main1
    "getopt-func"          -> main2A
    "getopt-kleisli"       -> main2B
    "optparse-applicative" -> main3



-- System.Console.GetOpt
------------------------

{-
This is mostly a port of GNU's `getopt` C library. It's been around a while, which depending on how I'm feeling means that either (1) it is more established or (2) it may not take advantage of new ideas. Either way, it does what it does. There are really only two things to know: the `getOpt` function and the `OptDescr` type. The definition of `OptDescr` looks like this, and represents the details of a command line option parameterized over the description type `a` (we will see how to use that later).
-}

-- data OptDescr a =      -- description of a single options:
--    Option [Char]       --   list of short option characters
--           [String]     --   list of long option strings (without "--")
--           (ArgDescr a) --   argument descriptor
--           String       --   explanation of option for user

{-
Given a list of option descriptions, `getOpt` takes the list of CL arguments from `getArgs` and tries to extract the `a`s it knows how to parse. It also outputs a list of the options it could not recognize and a list of any parse errors in the options (not including parse errors imposed by your code, just ones involving the option format).
-}

-- getOpt :: ArgOrder a                   -- non-option handling
--        -> [OptDescr a]                 -- option descriptors
--        -> [String]                     -- the command-line arguments
--        -> ([a],[String],[String])      -- (options,non-options,error messages)

{-
We write `getOptions` as a uniform way to process options into `a`s, so we can more easily see the differences when using different `a`.
-}
getOptions :: [OptDescr a] -> IO [a]
getOptions optDescr = do
  argv <- getArgs
  case getOpt Permute optDescr argv of
    -- no errors, no unrecognized options
    (flags, [], []) -> pure flags

    -- unrecognized options
    (_, nonopts, []) -> do
      putStrLn $ "Unrecognized: " <> show nonopts
      exitFailure

    -- option errors
    (_, _, errs) -> do
      putStrLn $ "Errors: " <> show errs
      exitFailure

{-
The last piece we need is `ArgDescr`. Options come in three flavors: those which do not require or accept an argument (called "flags"), those which accept but do not require an argument (which have "defaults"), and those which both accept and require an argument. For each case our job is to construct an `a` from whatever the argument string is.
-}

-- data ArgDescr a
--    = NoArg                   a         -- ^ no argument expected
--    | ReqArg (String       -> a) String -- ^ option requires argument
--    | OptArg (Maybe String -> a) String -- ^ optional argument

{-
So. This makes sense at a high level, but what exactly are we supposed to use for `a`? From the signature of `getOpt`, we end up with a list of `a`s that we'll need to interpret. In an effort to be systematic, we'll look at two possibilities: whether or not the outermost type constructor appearing in `a` is an arrow (`->`).
-}

-- Case 1: Outer constructor of `a` is not an arrow.

{-
In this case the `a`s all have to represent different options as closed terms. So right away we need a type that includes multiple things that have nothing to do with each other. Sums do this.
-}

data Option_GetOpt1
  = Help_GetOpt1
  | Input_GetOpt1 String
  | Shout_GetOpt1 (Maybe String)
  deriving (Show)

options1 :: [OptDescr Option_GetOpt1]
options1 =
  [ Option ['?'] ["help"] (NoArg Help_GetOpt1)
      "show usage"

  , Option [] ["input"] (ReqArg Input_GetOpt1 "FILE")
      "path to input file; default is stdin"

  , Option [] ["shout"] (OptArg Shout_GetOpt1 "STRING")
      "shout any occurrences of the argument"
  ]

main1 :: IO ()
main1 = do
  opts <- getOptions options1
  print opts

{-
I think of this as the "flag bag" approach, because after processing we just have a big list of values. There may be duplicates. This is fine for simple tools, but I'm not a big fan otherwise. Very often there are constraints on arguments like "must be unique", and with a flag bag we have to verify all that stuff manually. There is a better way.
-}

-- Case 2: Outer constructor of `a` is an arrow.

{-
In this case we end up with something like `[u -> v]` after processing the options. I can think of two related ways to combine a list of functions into a non-function value, both of which involve composing all the functions together (and thus require the domain and codomain be the same). We can think of the arrows as living in the usual category Hask, or in the Kleisli category over Hask relative to some monad m. In either case we need a type for the options object. Remember that the `--shout` parameter can appear multiple times, which we have to account for in our option record.
-}

data Option_GetOpt2 = Option_GetOpt2
  { og2_help  :: Bool
  , og2_input :: Maybe FilePath
  , og2_shout :: [Maybe String]
  } deriving (Show)

defaultOpts2 :: Option_GetOpt2
defaultOpts2 = Option_GetOpt2
  { og2_help  = False
  , og2_input = Nothing
  , og2_shout = []
  }

-- Hask
options2A :: [OptDescr (Option_GetOpt2 -> Option_GetOpt2)]
options2A =
  [ Option ['?'] ["help"] (NoArg $ \opts -> opts { og2_help = True })
      "show usage"

  , Option [] ["input"] (ReqArg (\path opts -> opts { og2_input = Just path }) "FILE")
      "path to input file; default is stdin"

  , Option [] ["shout"] (OptArg (\arg opts -> opts { og2_shout = arg : og2_shout opts }) "STRING")
      "shout any occurrences of the argument"
  ]

main2A :: IO ()
main2A = do
  fs <- getOptions options2A
  let opts = foldr (.) id fs defaultOpts2
  print opts

{-
Using a Kleisli category allows for interleaving validation with option parsing. In this case I've chosen IO just for the sake of concreteness; it could have been any monad depending on what features we want. Being in IO means we can test the input file for existence while processing options.
-}

-- Kleisli
options2B :: [OptDescr (Option_GetOpt2 -> IO Option_GetOpt2)]
options2B =
  [ Option ['?'] ["help"] (NoArg $ \opts -> pure $ opts { og2_help = True })
      "show usage"

  , let
      munge path opts = do
        fileExists <- doesFileExist path
        case fileExists of
          True  -> pure $ opts { og2_input = Just path }
          False -> putStrLn "File does not exist!" >> exitFailure
    in Option [] ["input"] (ReqArg munge "FILE")
      "path to input file; default is stdin"

  , Option [] ["shout"] (OptArg (\arg opts -> pure $ opts { og2_shout = arg : og2_shout opts }) "STRING")
      "shout any occurrences of the argument"
  ]

main2B :: IO ()
main2B = do
  fs <- getOptions options2B
  opts <- foldr (>=>) pure fs defaultOpts2
  print opts

{-
There's another approach I just realized: we could put wrapped values like `m a` in the flag bag. I've never tried this but am skeptical that it offers much of an improvement over the others.
-}



-- optparse-applicative
-----------------------

{-
System.Console.GetOpt is part of `base`, which means using it avoids a little bit of dependency overhead. There is a more modern library called `optparse-applicative`. It follows the same basic strategy as `System.Console.GetOpt`, that is, you build a static description of your options and the library handles the parsing. The big difference is that the `Parser` type constructor used for this is an applicative functor but not a monad, so parsers are built using only the applicative operators. This is significant because it removes all the "causal influence" between different parts of the parser--the parse result at one option cannot depend on what the result was at an earlier option. Consequentially, applicative parsers are amenable to much stronger forms of static analysis, which `optparse-applicative` leverages to support features like shell completion.
-}

{-
The main function for getting options is `execParser` (it handles calling `getArgs` internally):
-}

-- execParser :: ParserInfo a -> IO a

{-
`ParserInfo a`s are built using `info`:
-}

-- info :: Parser a -> InfoMod a -> ParserInfo a

{-
`InfoMod a` is literally the monoid of functions on `ParserInfo a`:
-}

-- newtype InfoMod a = InfoMod
--   { applyInfoMod :: ParserInfo a -> ParserInfo a }

{-
There are three basic atomic parsers, which are combined into more complex parsers using the applicative interface.
-}

-- flag     :: a -> a -> Mod FlagFields a -> Parser a
-- option   :: ReadM a -> Mod OptionFields a -> Parser a
-- argument :: ReadM a -> Mod ArgumentFields a -> Parser a -- positional arguments

{-
The `Mod f a` type is another monoid, used to modify parsers. There are atomic modifiers to set the long and short option names, hide some detail in the help text, and a lot more. The `ReadM a` type has some atomic readers: `str`, `maybeReader`, `eitherReader`, and some others. Let's try an example:
-}

data Option_GetOpt3 = Option_GetOpt3
  { og3_help  :: Bool
  , og3_input :: Maybe FilePath
  , og3_shout :: [Maybe String]
  } deriving (Show)

options3 :: Parser Option_GetOpt3
options3 = Option_GetOpt3
  <$> (flag False True (short '?' <> long "help"))
  <*> option (maybeReader (Just . Just)) (long "input")
  <*> many (option (maybeReader (Just . Just)) (long "shout"))

main3 :: IO ()
main3 = do
  opts <- execParser (info options3 fullDesc)
  print opts

{-
This parser has a bug, which arguably is bad API design. The specification says that the `--shout` parameter's argument is optional /and/ that `--shout` can appear multiple times. These two properties are unfortunately incompatible, because parsers that cannot fail (which becomes the case when it has a default value) will hang when passed to `some` or `many`. There may be a way to fix this that I just can't think of.
-}
