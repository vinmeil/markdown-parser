module Assignment (markdownParser, convertADTHTML) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..), ParseError (..), ParseResult (..))
import           Parser

import           Control.Applicative
import           Data.Functor     (($>))
import Data.List (isPrefixOf)


-- BNF
-- <Number>        ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
-- <Char>          ::= any character honestly
-- <Text>          ::= <Char> | <Char> <Text>
-- <URL>           ::= <Text>
-- <Italic>        ::= '_' <Text> '_'
-- <Bold>          ::= '**' <Text> '**'
-- <Strikethrough> ::= '~~' <Text> '~~'
-- <Link>          ::= '[' <Text> ']' '(' <URL> ')'
-- <InlineCode>    ::= '`' <Text> '`'
-- <Footnote>      ::= '[^' <Number> ']'
-- <Image>         ::= '!' '[' <Text> ']' '(' <URL>  '"' <Text> '"' ')'
-- <FootnoteRef>   ::= <Footnote> ':' <Text>
--
--
--
--

data ADT = Empty |
  -- Your ADT **must** derive Show.
  PlainText String |
  Italic String |
  Bold String |
  Strikethrough String
  deriving (Show, Eq)

-- == HELPER FUNCTIONS == --

-- helper function to turn `char` result into string
-- its to avoid having type errors of expected [Char], actual Char
charToString :: Parser String
charToString = (:[]) <$> char

-- checks result of parsing using string, might be reused in
-- the future, so ill just put it in a function
-- more like a boolean check if you ask me
checkString :: [String] -> Parser String
checkString strs = Parser $ \input ->
  if any (`isPrefixOf` input) strs
    then Result input ""
    else Error UnexpectedEof

parseUntil :: String -> Parser String
parseUntil str = f
  where
    -- use recursion to concat letter by letter until it finds the string
    f = checkString [str] <|> ((++) <$> charToString <*> f)

-- parses string until a specified string is found
getStringBetween :: String -> Parser String
getStringBetween str = string str *> parseUntil str <* string str
    --          ^ base case


-- == PARSERS == --

-- parses string into italic adt
italic :: Parser ADT
italic = Italic <$> getStringBetween "_"

-- parses string into bold adt
bold :: Parser ADT
bold = Bold <$> getStringBetween "**"

-- parses string into strikethrough adt
strikethrough :: Parser ADT
strikethrough = Strikethrough <$> getStringBetween "~~"

-- plainText :: Parser ADT
-- plainText = PlainText <$> (parseUntil "_" <|> parseUntil "**" <|> parseUntil "~~")

-- plainText :: Parser ADT
-- plainText = PlainText <$> f
--   where
--     f = checkString ["_", "**", "~~"] $> "" <|> ((:) <$> char <*> f)

plainText :: Parser ADT
plainText = PlainText <$> f
  where
    f = do
      c <- char
      rest <- checkString ["_", "**", "~~"] <|> eof $> "" <|> f
      return (c : rest)


-- == MAIN PARSER == --

-- parses text into adt
parseModifiers :: Parser ADT
parseModifiers = italic <|> bold <|> strikethrough

parseText :: Parser [ADT]
parseText = many (parseModifiers <|> plainText)


markdownParser :: Parser ADT
markdownParser = pure Empty

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = "IMPLEMENT_THIS"
