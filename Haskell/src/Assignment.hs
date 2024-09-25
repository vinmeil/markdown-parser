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


parseUntil :: String -> Parser String
parseUntil str = f (0 :: Int)
  -- use recursion to concat letter by letter until it finds the string
  where
    f 0 = (:) <$> char <*> f 1 -- make sure it has at least 1 letter before checking
    f len = (isParserSucceed (string str) $> "") <|> ((:) <$> char <*> f (len + 1))


-- parses string until a specified string is found
getStringBetween :: String -> Parser String
getStringBetween str = string str *> parseUntil str <* string str


-- checks if a parser returns an error or not without consuming input
isParserSucceed :: Parser a -> Parser ()
isParserSucceed (Parser p) = Parser $ \input ->
  case p input of
    Result _ _ -> Result input ()
    Error _    -> Error UnexpectedEof




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

-- parses text into adt
parseModifiers :: Parser ADT
parseModifiers = italic <|> bold <|> strikethrough


-- parses string into plain text adt
plainText :: Parser ADT
plainText = PlainText <$> f
  where
    f = (:) <$> char <*> ((isParserSucceed parseModifiers $> "") <|> eof $> "" <|> f)

    -- f = do
    --   c <- char -- consume first character because we know it failed modifiers
    --   rest <- (isParserSucceed parseModifiers $> "") <|> eof $> "" <|> f
    --   return (c : rest)


-- == MAIN PARSER == --

parseText :: Parser [ADT]
parseText = many (parseModifiers <|> plainText)


markdownParser :: Parser ADT
markdownParser = pure Empty

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = "IMPLEMENT_THIS"
