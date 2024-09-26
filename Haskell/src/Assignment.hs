module Assignment (markdownParser, convertADTHTML) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..), ParseError (..), ParseResult (..))
import           Parser

import           Control.Applicative
import           Data.Functor     (($>))
import Control.Monad (guard)


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
  Strikethrough String |
  Link (String, String) |
  Code String |
  Footnote Int |
  Image (String, String, String) |
  FootnoteRef (Int, String) |
  Heading (Int, String)
  -- Footnote String
  deriving (Show, Eq)

-- == HELPER FUNCTIONS == --


parseUntil :: String -> Parser String
parseUntil str = f (0 :: Int)
  -- use recursion to concat letter by letter until it finds the string
  where
    f 0 = (:) <$> char <*> f 1 -- ensure has at least 1 letter before checking
    f len = (isParserSucceed (string str) $> "") <|> ((:) <$> char <*> f (len + 1))


parseUntilInlineSpace :: Parser String
parseUntilInlineSpace = f
  where
    f = do
      whitespace <- inlineSpace
      if not (null whitespace) then pure ""
        else do (:) <$> char <*> f


parseUntilNewline :: Parser String
parseUntilNewline = parseUntil "\n"


-- parses string until a specified string is found
getStringBetween :: String -> String -> Parser String
getStringBetween st1 st2 = string st1 *> parseUntil st2 <* string st2


-- checks if a parser returns an error or not without consuming input
isParserSucceed :: Parser a -> Parser ()
isParserSucceed (Parser p) = Parser $ \input ->
  case p input of
    Result _ _ -> Result input ()
    Error _    -> Error UnexpectedEof




-- == PARSERS == --

-- parses string into italic adt
italic :: Parser ADT
italic = Italic <$> getStringBetween "_" "_"

-- parses string into bold adt
bold :: Parser ADT
bold = Bold <$> getStringBetween "**" "**"

-- parses string into strikethrough adt
strikethrough :: Parser ADT
strikethrough = Strikethrough <$> getStringBetween "~~" "~~"

-- parses string into link adt
link :: Parser ADT
link = do
  text <- getStringBetween "[" "]"
  _ <- inlineSpace
  url <- getStringBetween "(" ")"
  return $ Link (text, url)

-- parses string into code adt
code :: Parser ADT
code = Code <$> getStringBetween "`" "`"

-- gets the number inside of a footnote
getFootnoteNumber :: Parser Int
getFootnoteNumber = do
  _ <- string "[^"
  spacesBefore <- inlineSpace
  guard (null spacesBefore) <|> unexpectedStringParser "Expected no spaces"
  num <- int
  guard (num > 0) <|> unexpectedStringParser "Expected positive integer"
  spacesAfter <- inlineSpace
  guard (null spacesAfter) <|> unexpectedStringParser "Expected no spaces"
  _ <- string "]"
  return num

-- parses string into footnote adt
footnote :: Parser ADT
footnote = Footnote <$> getFootnoteNumber

-- parses string into image adt
image :: Parser ADT
image = do
  _ <- string "!"
  text <- getStringBetween "[" "]"
  _ <- inlineSpace *> string "("
  url <- parseUntilInlineSpace
  caption <- getStringBetween "\"" "\""
  _ <- string ")"
  return $ Image (text, url, caption)

-- parses string into footnote reference adt
footnoteRef :: Parser ADT
footnoteRef = do
  _ <- inlineSpace -- remove leading spaces
  num <- getFootnoteNumber
  _ <- string ":" *> inlineSpace -- remove spaces after ":"
  text <- parseUntilNewline <* string "\n" -- remove newline character
  return $ FootnoteRef (num, text)

-- parses string into heading adt
headingHashtag :: Parser ADT
headingHashtag = do
  _ <- inlineSpace
  len <- length <$> some (is '#')
  guard (len <= 6) <|> unexpectedStringParser "Expected only 1-6 hashtags"
  _ <- inlineSpace
  text <- parseUntilNewline <* string "\n"
  return $ Heading (len, text)



-- parses text into adt
parseModifiers :: Parser ADT
parseModifiers = italic <|>
                 bold <|>
                 strikethrough <|>
                 link <|>
                 code <|>
                 footnoteRef <|>
                 footnote <|>
                 image


-- parses string into plain text adt
plainText :: Parser ADT
plainText = PlainText <$> f
  where
    f = do
      c <- char -- consume first character because we know it failed modifiers
      rest <- (isParserSucceed parseModifiers $> "") <|> eof $> "" <|> f
      return (c : rest)


-- == MAIN PARSER == --

parseText :: Parser [ADT]
parseText = do
  _ <- optional inlineSpace
  parsedTexts <- many (parseModifiers <|> plainText)
  _ <- optional inlineSpace
  _ <- optional (string "\n")
  return parsedTexts


markdownParser :: Parser ADT
markdownParser = pure Empty

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = "IMPLEMENT_THIS"
