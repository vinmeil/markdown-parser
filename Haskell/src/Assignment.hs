module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative
import Control.Monad (guard)
import Data.Functor (($>))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (ParseError (..), ParseResult (..), Parser (..))
import Parser

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

data ADT
  = Empty
  | -- Your ADT **must** derive Show.
    JustText String
  | Newline Char
  | Paragraph String
  | Italic String
  | Bold String
  | Strikethrough String
  | Link (String, String)
  | InlineCode String
  | Footnote Int
  | Image (String, String, String)
  | FootnoteRef (Int, String)
  | Heading (Int, ADT)
  | Blockquote [ADT]
  -- Footnote String
  deriving (Show, Eq)

-- == HELPER FUNCTIONS == --

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

parseUntil :: String -> Parser String
parseUntil str = f (0 :: Int)
  where
    -- use recursion to concat letter by letter until it finds the string

    f 0 = (:) <$> char <*> f 1 -- ensure has at least 1 letter before checking
    f len = (isParserSucceed (string str) $> "") <|> ((:) <$> char <*> f (len + 1))

parseUntilNotChar :: Char -> Parser String
parseUntilNotChar chr = some (is chr)

-- should return an error if theres more
parseAtMost :: Int -> Char -> Parser String
parseAtMost lim ch = do
  str <- some (is ch)
  guard (length str <= lim) <|> unexpectedStringParser "Parsed too many characters"
  return str

parseAtLeast :: Int -> Char -> Parser String
parseAtLeast lim ch = do
  str <- some (is ch)
  guard (length str >= lim) <|> unexpectedStringParser "Not enough characters"
  return str

parseUntilEof :: Parser String
parseUntilEof = many char

parseUntilInlineSpace :: Parser String
parseUntilInlineSpace = f
  where
    f = do
      whitespace <- inlineSpace
      if not (null whitespace)
        then pure ""
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
    Error _ -> Error UnexpectedEof

-- checks if theres a newline and removes whitespaces after
checkNewlineAndRemoveSpace :: Parser String
checkNewlineAndRemoveSpace = string "\n" *> inlineSpace

-- == PARSERS == --

-- my own parsers --

newline :: Parser ADT
newline = Newline <$> is '\n'

justText :: Parser ADT
justText = JustText <$> (parseUntilNewline <|> parseUntilEof)

--------------------
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
link =
  Link
    <$> ( (,)
            <$> (getStringBetween "[" "]" <* inlineSpace)
            <*> getStringBetween "(" ")"
        )

-- parses string into inlinecode adt
inlinecode :: Parser ADT
inlinecode = InlineCode <$> getStringBetween "`" "`"

-- gets the number inside of a footnote
getFootnoteNumber :: Parser Int
getFootnoteNumber = do
  _ <- string "[^"
  spacesBefore <- inlineSpace
  num <- int
  spacesAfter <- inlineSpace
  guard (null spacesAfter && (num > 0) && null spacesBefore)
    <|> unexpectedStringParser "Expected no spaces and positive integer"
  _ <- string "]"
  return num

-- parses string into footnote adt
footnote :: Parser ADT
footnote = Footnote <$> getFootnoteNumber

-- NON MODIFIER PARSERS

-- parses string into image adt
image :: Parser ADT
image =
  Image
    <$> ( (,,)
            <$> getStringBetween "![" "]"
            <* inlineSpace
            <*> (string "(" *> parseUntilInlineSpace)
            <*> (getStringBetween "\"" "\"" <* string ")")
        )

-- parses string into footnote reference adt
footnoteRef :: Parser ADT
footnoteRef =
  FootnoteRef
    <$> ( (,)
            <$> ( getFootnoteNumber
                    <* string ":"
                    <* inlineSpace
                )
            <*> parseUntilNewline
            <* is '\n'
        )

-- parses string into heading adt
headingHashtag :: Parser ADT
headingHashtag =
  Heading
    <$> ( (,)
            <$> (length <$> parseAtMost 6 '#')
            <*> (parseModifiers <|> justText)
        )

-- helper function to parse alternative headings
altHeading :: Int -> Char -> Parser ADT
altHeading n c =
  Heading
    <$> ( flipTuple
            <$> ( (,)
                    <$> (parseModifiers <|> justText)
                    <* is '\n'
                    <*> (parseAtLeast 2 c $> n)
                )
        )

altHeading1 :: Parser ADT
altHeading1 = altHeading 1 '='

altHeading2 :: Parser ADT
altHeading2 = altHeading 2 '-'

heading :: Parser ADT
heading = headingHashtag <|> altHeading1 <|> altHeading2

-- parses string into blockquote adt
blockquote :: Parser ADT
blockquote =
  Blockquote
    <$> some
      ( inlineSpace
          *> charTok '>'
          *> (paragraph <* optional (is '\n'))
      )

-- parses text into adt
parseModifiers :: Parser ADT
parseModifiers =
  italic
    <|> bold
    <|> strikethrough
    <|> link
    <|> inlinecode
    <|> footnote

-- parses string into paragraph adt
paragraph :: Parser ADT
paragraph = Paragraph <$> f
  where
    f = do
      c <- char -- consume first character because we know it failed modifiers
      rest <-
        (isParserSucceed (parseModifiers <|> newline) $> "")
          <|> eof $> ""
          <|> f
      return (c : rest)

parseNonModifiers :: Parser ADT
parseNonModifiers =
  inlineSpace
    *> ( footnoteRef
           <|> image
           <|> heading
           <|> blockquote
       )

-- == MAIN PARSER == --

parseText :: Parser [ADT]
parseText = do
  _ <- allSpace
  many
    (newline <|> parseNonModifiers <|> parseModifiers <|> paragraph)

markdownParser :: Parser ADT
markdownParser = pure Empty

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = "IMPLEMENT_THIS"
