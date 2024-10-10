module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative
import Control.Exception (try)
import Control.Monad (guard)
import Data.Char (toUpper)
import Data.Functor (($>))
import Data.List
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Debug.Trace (trace)
import Instances (ParseError (..), ParseResult (..), Parser (..))
import Parser

-- BNF
-- <Number>        ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
-- <Char>          ::= any character honestly
-- <Text>          ::= <AnyChar> | <AnyChar> <Text>
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
  | Heading (Int, [ADT])
  | Blockquote [[ADT]]
  | Code (String, String)
  | OrderedList [(Int, [ADT])]
  | -- | Table [ADT]
    Table [[ADT]]
  | -- | TableHeader [ADT]
    TableHeader [ADT]
  | -- | TableRow [ADT]
    TableRow [ADT]
  | TableCell [ADT]
  -- Footnote String
  deriving (Show, Eq)

modifierPrefixes :: [String]
-- modifierPrefixes = ["_", "**", "~~", "[", "`", "[^", "![", "\n", ">"]
modifierPrefixes = ["|", "_", "**", "~~", "[", "`", "[^", "![", "\n", ">"]

-- == HELPER FUNCTIONS == --

positiveInt :: Parser Int
positiveInt = do
  num <- int
  guard (num > 0) <|> unexpectedStringParser "Expected positive integer"
  return num

startsWith :: String -> Parser ()
startsWith str = isParserSucceed (string str)

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

parseUntilModifier :: Parser String
parseUntilModifier = f
  where
    f = do
      c <- char -- consume first character because we know it failed modifiers
      rest <-
        (asum (map startsWith modifierPrefixes) $> "")
          -- <|> eof $> ""
          <|> f
      return (c : rest)

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
parseUntilEof = some char

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

parseModifierAndTextUntilNewline :: Parser [ADT]
parseModifierAndTextUntilNewline =
  some
    ( parseModifiers
        <|> (isParserSucceed (isNot '\n') *> justText)
    )

parseModifierAndTextUntil :: Char -> Parser [ADT]
parseModifierAndTextUntil delim =
  some
    ( parseModifiers
        <|> ( isParserSucceed (isNot delim)
                *> ( JustText
                       <$> ( parseUntilModifier
                               <|> parseUntilNewline
                               <|> parseUntilEof
                               <|> parseUntil [delim]
                           )
                   )
            )
    )

-- parseUntilSpacesAndDelim :: String -> Parser String
-- parseUntilSpacesAndDelim delim = f
--   where
--     f = (isParserSucceed (inlineSpace *> string delim) *> (inlineSpace *> string delim $> "")) <|> ((:) <$> char <*> f)

parseUntilSpacesAndDelim :: String -> Parser String
parseUntilSpacesAndDelim delim = f
  where
    f = (isParserSucceed (inlineSpace *> string delim) *> trace "removing space and delim" (inlineSpace $> "")) <|> ((:) <$> trace "getting next char" char <*> f)

fun :: String -> Parser String
fun delim = f
  where
    f = (isParserSucceed (inlineSpace *> string delim) *> (inlineSpace *> string delim $> "")) <|> ((:) <$> char <*> f)

-- == PARSERS == --

-- my own parsers --

newline :: Parser ADT
newline = Newline <$> is '\n'

justText :: Parser ADT
justText = JustText <$> (parseUntilModifier <|> parseUntilNewline <|> parseUntilEof)

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
  num <- positiveInt
  spacesAfter <- inlineSpace
  guard (null spacesAfter && null spacesBefore)
    <|> unexpectedStringParser "Expected no spaces and positive integer"
  _ <- string "]"
  return num

-- parses string into footnote adt
footnote :: Parser ADT
footnote = Footnote <$> getFootnoteNumber

-- NON MODIFIER PARSERS

-- parses string into image adt
-- image :: Parser ADT
-- image =
--   Image
--     <$> ( (,,)
--             <$> getStringBetween "![" "]"
--             <* inlineSpace
--             <*> (string "(" *> parseUntilInlineSpace)
--             <*> (getStringBetween "\"" "\"" <* string ")")
--         )

image :: Parser ADT
image = do
  alt <- getStringBetween "![" "]"
  _ <- inlineSpace
  url <- string "(" *> parseUntilInlineSpace
  caption <- getStringBetween "\"" "\"" <* string ")"
  return $ Image (alt, url, caption)

-- parses string into footnote reference adt
-- footnoteRef :: Parser ADT
-- footnoteRef =
--   FootnoteRef
--     <$> ( inlineSpace
--             *> ( (,)
--                    <$> ( getFootnoteNumber
--                            <* string ":"
--                            <* inlineSpace
--                        )
--                    <*> parseUntilNewline
--                    <* is '\n'
--                )
--         )

footnoteRef :: Parser ADT
footnoteRef = do
  _ <- inlineSpace
  num <- getFootnoteNumber <* string ":" <* inlineSpace
  content <- (parseUntilNewline <|> parseUntilEof) <* optional (is '\n')
  return $ FootnoteRef (num, content)

-- parses string into heading adt
-- headingHashtag :: Parser ADT
-- headingHashtag =
--   Heading
--     <$> ( (,)
--             <$> (length <$> parseAtMost 6 '#' <* inlineSpace1)
--             <*> parseModifierAndTextUntilNewline
--         )

headingHashtag :: Parser ADT
headingHashtag = do
  level <- length <$> parseAtMost 6 '#' <* inlineSpace1
  content <- parseModifierAndTextUntilNewline
  return $ Heading (level, content)

-- helper function to parse alternative headings
-- altHeading :: Int -> Char -> Parser ADT
-- altHeading n c =
--   Heading
--     <$> ( flipTuple
--             <$> ( (,)
--                     <$> parseModifierAndTextUntilNewline
--                     <* is '\n'
--                     <*> (parseAtLeast 2 c $> n)
--                 )
--         )

altHeading :: Int -> Char -> Parser ADT
altHeading n c = do
  content <- parseModifierAndTextUntilNewline <* is '\n'
  _ <-
    parseAtLeast 2 c
      -- remove spaces and \n or eof after dashes, if it fails then theres
      -- another character so it fails the parser
      <* inlineSpace
      <* ((is '\n' <|> (eof $> ' ')) <|> unexpectedStringParser "Unexpected characters after heading")
  return $ Heading (n, content)

altHeading1 :: Parser ADT
altHeading1 = altHeading 1 '='

altHeading2 :: Parser ADT
altHeading2 = altHeading 2 '-'

heading :: Parser ADT
heading = inlineSpace *> (headingHashtag <|> altHeading1 <|> altHeading2)

-- parses string into blockquote adt
-- blockquote :: Parser ADT
-- blockquote =
--   Blockquote
--     <$> some
--       ( inlineSpace
--           *> charTok '>'
--           *> parseModifierAndTextUntilNewline
--           <* optional newline
--       )

blockquote :: Parser ADT
blockquote = do
  quotes <- some $ do
    _ <- inlineSpace
    _ <- charTok '>'
    adts <- parseModifierAndTextUntilNewline
    _ <- optional newline
    return adts
  return $ Blockquote quotes

-- Helper function for code block to recursively parse until closing code block
parseUntilClosingCode :: Parser String
parseUntilClosingCode =
  (parseUntil "```\n" <* string "```\n")
    <|> (parseUntil "\n```" <* string "\n```" <* eof)
    <|> ((:) <$> char <*> parseUntilClosingCode)

-- parses string into code block adt
-- code :: Parser ADT
-- code =
--   Code
--     <$> ( (,)
--             <$> (string "```\n" $> "" <|> getStringBetween "```" "\n")
--             <*> parseUntilClosingCode
--         )

code :: Parser ADT
code = do
  opening <- string "```\n" $> "" <|> getStringBetween "```" "\n"
  content <- parseUntilClosingCode
  return $ Code (opening, content)

-- helper function for ordered list to check for proper indentation
guardIndentation :: String -> Parser ()
guardIndentation indent = do
  -- make sure theres no other whitespace
  _ <- isParserSucceed (noneof "\t\r\f\v\n") <|> unexpectedStringParser "Only expected spaces"
  whitespace <- many (is ' ')
  guard (whitespace == indent) <|> unexpectedStringParser "Unexpected number of spaces"

-- helper function for ordered list to parse a line of an ordered list
parseListItemAux :: String -> Parser a -> Parser (a, [ADT])
parseListItemAux indent parser = do
  _ <- guardIndentation indent
  num <- parser -- gets the number at the front
  -- parses text and checks if theres a sublist
  textADTs <- parseModifierAndTextUntilNewline <* optional (is '\n')
  subList <- parseSublist (indent ++ "    ") <|> pure Empty
  let tupleSnd = textADTs ++ [subList] -- concats the sublist to the list
  return (num, tupleSnd)

-- helper function for ordered list to parse list items which calls an aux function
parseListItem :: Int -> String -> Parser (Int, [ADT])
parseListItem 1 indent = parseListItemAux indent ((string "1. " <* inlineSpace) $> 1)
parseListItem _ indent = parseListItemAux indent (positiveInt <* string ". " <* inlineSpace)

-- parses sublist
-- parseSublist :: String -> Parser ADT
-- parseSublist indent =
--   OrderedList
--     <$> ( (:)
--             <$> parseListItem 1 indent
--             <*> many (parseListItem 0 indent)
--         )
parseSublist :: String -> Parser ADT
parseSublist indent = do
  firstItem <- parseListItem 1 indent
  rest <- many (parseListItem 0 indent)
  return $ OrderedList (firstItem : rest)

-- parses string into orderedlist ADT
-- orderedList :: Parser ADT
-- orderedList =
--   OrderedList
--     <$> ( (:)
--             <$> parseListItem 1 ""
--             <*> many (parseListItem 0 "")
--         )

orderedList :: Parser ADT
orderedList = do
  firstItem <- parseListItem 1 ""
  rest <- many (parseListItem 0 "")
  return $ OrderedList (firstItem : rest)

-- parses string into table adt
validateDashes :: [ADT] -> Bool
validateDashes = all isValidDashCell
  where
    isValidDashCell (TableCell [JustText dashes]) = all (== '-') dashes && length dashes >= 3
    isValidDashCell _ = False

table :: Parser ADT
table = do
  header <- parseHeader
  dashes <- parseRow -- removes the separating line
  rows <- some parseRow
  return (Table ([header] : [rows]))

parseHeader :: Parser ADT
parseHeader = TableHeader <$> parseTableRow

parseRow :: Parser ADT
parseRow = TableRow <$> parseTableRow

parseTableRow :: Parser [ADT]
parseTableRow =
  charTok '|'
    *> parseTableCell `sepBy1` charTok '|'
    <* optional (is '\n')

parseTableCell :: Parser ADT
parseTableCell = do
  adts <- some $ do
    -- get a list of adts
    _ <- isParserSucceed (isNot '|')
    parseModifiers <|> (JustText <$> (parseUntilModifier <|> parseUntil "|"))
  return $ TableCell (fmap trimADT adts)

----------------------------------------------------------------------
-- got this function from github copilot, did not make this on my own.
trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse

----------------------------------------------------------------------

trimADT :: ADT -> ADT
trimADT (JustText str) = JustText (trim str)
trimADT (Italic str) = Italic (trim str)
trimADT (Bold str) = Bold (trim str)
trimADT (Strikethrough str) = Strikethrough (trim str)
trimADT (InlineCode str) = InlineCode (trim str)
trimADT (Paragraph str) = Paragraph (trim str)
trimADT _ = Empty

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
paragraph = Paragraph <$> (parseUntilModifier <|> parseUntilNewline <|> parseUntilEof)

parseNonModifiers :: Parser ADT
parseNonModifiers =
  footnoteRef
    <|> image
    <|> heading
    <|> blockquote
    <|> code
    <|> orderedList

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

-- HTML stuff

tag :: String -> String -> String
tag t content = "<" ++ t ++ ">" ++ content ++ "</" ++ t ++ ">"

altTag :: String -> String
altTag content = "<" ++ content ++ "/>"

-- htmlItalic :: Parser ADT -> String
