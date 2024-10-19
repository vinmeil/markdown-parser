module Assignment (markdownParser, convertADTHTML, convertADTHTMLBoilerplate, getTime) where

import Control.Applicative
import Control.Monad (guard, mfilter)
import Data.Char (toUpper)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Debug.Trace (trace)
import Instances (ParseError (..), ParseResult (..), Parser (..))
import Parser

-- BNF
-- <Number>        ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
-- <Char>          ::= any character honestly
-- <Text>          ::= <Char> | <Char> <Text>
-- <Italic>        ::= '_' <Text> '_'
-- <Bold>          ::= '**' <Text> '**'
-- <Strikethrough> ::= '~~' <Text> '~~'
-- <Link>          ::= '[' <Text> ']' '(' <Text> ')'
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
  | JustText String
  | Newline
  | Paragraph [ADT]
  | Italic String
  | Bold String
  | Strikethrough String
  | Link (String, String)
  | InlineCode String
  | Footnote Int
  | Image (String, String, String)
  | FootnoteRef (Int, String)
  | Heading (Int, [ADT])
  | Blockquote [ADT]
  | Code (String, String)
  | OrderedList [ADT]
  | ListItem [ADT]
  | Table [ADT]
  | TableHeader [ADT]
  | TableRow [ADT]
  | TableRowCell [ADT]
  | TableHeaderCell [ADT]
  | Markdown [ADT]
  deriving (Show, Eq)

modifierPrefixes :: [String]
modifierPrefixes = ["|", "_", "**", "~~", "[", "`", "[^", "![", "\n", ">"]

-- == HELPER FUNCTIONS == --

positiveInt :: Parser Int
positiveInt = do
  _ <- isParserSucceed notSpace
  isParserSucceed (isNot '-') *> int

startsWith :: String -> Parser ()
startsWith str = isParserSucceed (string str)

parseUntil :: String -> Parser String
parseUntil str = f (0 :: Int)
  where
    -- use recursion to concat letter by letter until it finds the string

    f 0 = (:) <$> char <*> f 1 -- ensure has at least 1 letter before checking
    f len = (isParserSucceed (string str) $> "") <|> ((:) <$> char <*> f (len + 1))

parseUntilModifier :: Parser String
parseUntilModifier = f
  where
    f = do
      c <- char -- consume first character because we know it failed modifiers
      rest <- (asum (map startsWith modifierPrefixes) $> "") <|> f
      return (c : rest)

-- should return an error if theres more
parseAtMost :: Int -> Char -> Parser String
parseAtMost lim ch = do
  str <- some (is ch)
  guard (length str <= lim) <|> unexpectedStringParser "Parsed too many characters"
  return str

-- returns an error if theres less
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

-- == PARSERS == --

-- my own parsers --

newline :: Parser ADT
newline = is '\n' $> Newline

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
link = do
  text <- getStringBetween "[" "]" <* inlineSpace
  url <- getStringBetween "(" ")"
  return $ Link (text, url)

-- parses string into inlinecode adt
inlinecode :: Parser ADT
inlinecode = InlineCode <$> getStringBetween "`" "`"

-- gets the number inside of a footnote
getFootnoteNumber :: Parser Int
getFootnoteNumber = do
  _ <- string "[^"
  num <- positiveInt
  _ <- string "]"
  return num

-- parses string into footnote adt
footnote :: Parser ADT
footnote = Footnote <$> getFootnoteNumber

-- NON MODIFIER PARSERS

image :: Parser ADT
image = do
  alt <- getStringBetween "![" "]"
  _ <- inlineSpace
  url <- string "(" *> parseUntilInlineSpace
  caption <- getStringBetween "\"" "\"" <* string ")"
  return $ Image (alt, url, caption)

footnoteRef :: Parser ADT
footnoteRef = do
  _ <- inlineSpace
  num <- getFootnoteNumber <* string ":" <* inlineSpace
  content <- (parseUntilNewline <|> parseUntilEof) <* optional (is '\n')
  -- content <- Paragraph <$> (parseUntilNewline <|> parseUntilEof) <* optional (is '\n')
  return $ FootnoteRef (num, content)

headingHashtag :: Parser ADT
headingHashtag = do
  level <- length <$> parseAtMost 6 '#' <* inlineSpace1
  content <- parseModifierAndTextUntilNewline
  return $ Heading (level, content)

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

blockquote :: Parser ADT
blockquote = do
  quotes <- some $ do
    _ <- inlineSpace
    _ <- charTok '>'
    adts <- paragraph
    _ <- optional (is '\n')
    return adts
  return $ Blockquote quotes

-- Helper function for code block to recursively parse until closing code block
parseUntilClosingCode :: Parser String
parseUntilClosingCode =
  (parseUntil "```\n" <* string "```\n")
    <|> (parseUntil "\n```" <* string "\n```" <* eof)
    <|> ((:) <$> char <*> parseUntilClosingCode)

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
parseListItemAux :: String -> Parser a -> Parser [ADT]
parseListItemAux indent parser = do
  _ <- guardIndentation indent
  _ <- parser -- gets the number at the front
  textADTs <- parseModifierAndTextUntilNewline <* optional (is '\n')
  subList <- parseSublist (indent ++ "    ") <|> pure Empty
  let listItems = ListItem textADTs : [subList] -- concats the sublist to the list
  return listItems

-- helper function for ordered list to parse list items which calls an aux function
parseListItem :: Int -> String -> Parser [ADT]
parseListItem 1 indent = parseListItemAux indent (string "1. " <* inlineSpace)
parseListItem _ indent = parseListItemAux indent (positiveInt <* string ". " <* inlineSpace)

parseSublist :: String -> Parser ADT
parseSublist indent = do
  firstItem <- parseListItem 1 indent
  rest <- many (parseListItem 0 indent)
  return $ OrderedList (concat (firstItem : rest))

orderedList :: Parser ADT
orderedList = do
  firstItem <- parseListItem 1 ""
  rest <- many (parseListItem 0 "")
  return $ OrderedList (concat (firstItem : rest))

-- parses string into table adt
table :: Parser ADT
table = do
  (header, count) <- parseHeader
  _ <- parseSeparator -- removes the separating line
  rows <- some (parseRow count)
  return (Table (header : rows))

-- parses table header separator
parseSeparator :: Parser ()
parseSeparator = do
  _ <- charTok '|'
  _ <- (parseAtLeast 3 '-' <* inlineSpace) `sepBy1` charTok '|'
  _ <- optional (is '|')
  _ <- optional (is '\n')
  return ()

-- parses table header
parseHeader :: Parser (ADT, Int)
parseHeader = do
  _ <- optional (is '\n')
  (header, rowCount) <- parseHeaderCells 1
  _ <- string "|\n"
  return (TableHeader header, rowCount)

-- helper function to recursively parse header cells and count them
parseHeaderCells :: Int -> Parser ([ADT], Int)
parseHeaderCells n = do
  cell <- TableHeaderCell <$> parseTableCell
  rest <- (startsWith "|\n" $> ([], n)) <|> parseHeaderCells (n + 1)
  let (cells, count) = rest
  return (cell : cells, count)

parseRow :: Int -> Parser ADT
parseRow cellCount = TableRow <$> parseTableRow cellCount

parseTableRow :: Int -> Parser [ADT]
parseTableRow cellCount = do
  _ <- optional (is '\n')
  _ <- inlineSpace
  cells <- parseNCells cellCount
  _ <- string "|"
  return cells

parseNCells :: Int -> Parser [ADT]
parseNCells 1 = (: []) <$> (TableRowCell <$> parseTableCell)
parseNCells n = do
  cell <- TableRowCell <$> parseTableCell
  rest <- parseNCells (n - 1)
  return (cell : rest)

parseTableCell :: Parser [ADT]
parseTableCell = do
  _ <- charTok '|' -- consume the first |
  adts <- some $ do
    -- get a list of adts
    _ <- isParserSucceed (isNot '|')
    parseModifiers <|> (JustText <$> (parseUntilModifier <|> parseUntil "|"))
  return (fmap trimADT adts)

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
-- trimADT (Paragraph str) = Paragraph (trim str)
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
-- should return a list of something like [JustText "hi ", Bold "md", JustText "ofc"]
paragraph :: Parser ADT
paragraph = do
  Paragraph <$> parseModifierAndTextUntilNewline

-- paragraph = Paragraph <$> (parseUntilModifier <|> parseUntilNewline <|> parseUntilEof)

parseNonModifiers :: Parser ADT
parseNonModifiers =
  footnoteRef
    <|> image
    <|> heading
    <|> blockquote
    <|> code
    <|> orderedList
    <|> table

-- == MAIN PARSER == --

parseText :: Parser [ADT]
parseText = do
  _ <- allSpace
  many (newline <|> parseNonModifiers <|> paragraph)

markdownParser :: Parser ADT
markdownParser = Markdown <$> parseText

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

htmlFootnote :: Int -> String
htmlFootnote num = do
  let idAttr = htmlId ("fn" ++ show num ++ "ref")
  let hrefAttr = href ("#fn" ++ show num)
  let anchorTag = tagWithStringInside "a" (idAttr ++ " " ++ hrefAttr) (show num)
  tag "sup" anchorTag

htmlImage :: String -> String -> String -> String
htmlImage alt url caption =
  altTag
    "img"
    ( "src=\""
        ++ url
        ++ "\" alt=\""
        ++ alt
        ++ "\" title=\""
        ++ caption
        ++ "\""
    )
    ++ "\n"

htmlFootnoteRef :: Int -> String -> String
htmlFootnoteRef num content = tagWithId "p" ("fn" ++ show num) content ++ "\n"

htmlParagraph :: [ADT] -> String
htmlParagraph adts = tag "p" (concatMap convertADTHTML adts)

htmlHeading :: Int -> [ADT] -> String
htmlHeading n adts = tag ("h" ++ show n) (concatMap convertADTHTML adts) ++ "\n"

htmlCodeLanguage :: String -> String -> String
htmlCodeLanguage lang content = do
  let codeTag = tagWithStringInside "code" ("class=\"language-" ++ lang ++ "\"") content
  let preTag = tag "pre" codeTag
  preTag ++ "\n"

convertADTHTMLBoilerplate :: String -> ADT -> String
convertADTHTMLBoilerplate title adt = boilerplate [adt] title

convertADTHTML :: ADT -> String
convertADTHTML (Markdown adts) = indentInside "body" adts
convertADTHTML (JustText str) = str
convertADTHTML Newline = ""
convertADTHTML Empty = ""
convertADTHTML (Italic str) = tag "em" str
convertADTHTML (Bold str) = tag "strong" str
convertADTHTML (Strikethrough str) = tag "del" str
convertADTHTML (InlineCode str) = tag "code" str
convertADTHTML (Link (text, url)) = tagWithHref "a" url text
convertADTHTML (Footnote num) = htmlFootnote num
convertADTHTML (Image (alt, url, caption)) = htmlImage alt url caption
convertADTHTML (FootnoteRef (num, content)) = htmlFootnoteRef num content
convertADTHTML (Paragraph adts) = htmlParagraph adts
convertADTHTML (Heading (n, adts)) = htmlHeading n adts
convertADTHTML (Blockquote adts) = indentInside "blockquote" adts
convertADTHTML (Code ("", content)) = tag "pre" (tag "code" content) ++ "\n"
convertADTHTML (Code (opening, content)) = htmlCodeLanguage opening content
convertADTHTML (ListItem adts) = tag "li" (concatMap convertADTHTML adts)
convertADTHTML (OrderedList items) = indentInside "ol" items
convertADTHTML (Table rows) = indentInside "table" rows
convertADTHTML (TableHeader cells) = indentInside "tr" cells
convertADTHTML (TableRow cells) = indentInside "tr" cells
convertADTHTML (TableHeaderCell adts) = tag "th" (concatMap convertADTHTML adts)
convertADTHTML (TableRowCell adts) = tag "td" (concatMap convertADTHTML adts)

------------------------------------------------
-- got this code from github copilot. i did not make this on my own
indentInside :: String -> [ADT] -> String
indentInside t adts =
  openTag t
    ++ "\n"
    ++ intercalate "\n" (map (indentString . convertADTHTML) (filterNonEmpty adts))
    ++ "\n"
    ++ closeTag t
    ++ "\n"

indentString :: String -> String
indentString = intercalate "\n" . map ("    " ++) . lines

--
------------------------------------------------

openTag :: String -> String
openTag t = "<" ++ t ++ ">"

-- helper function to close a tag
closeTag :: String -> String
closeTag t = "</" ++ t ++ ">"

-- helper function to filter out Empty adts
filterNonEmpty :: [ADT] -> [ADT]
filterNonEmpty = filter (\x -> x /= Empty && x /= Newline)

tag :: String -> String -> String
tag t content = openTag t ++ content ++ closeTag t

altTag :: String -> String -> String
altTag t content = "<" ++ t ++ " " ++ content ++ ">"

href :: String -> String
href url = "href=\"" ++ url ++ "\""

htmlId :: String -> String
htmlId str = "id=\"" ++ str ++ "\""

tagWithHref :: String -> String -> String -> String
tagWithHref t url content =
  "<" ++ t ++ " " ++ href url ++ ">" ++ content ++ "</" ++ t ++ ">"

tagWithId :: String -> String -> String -> String
tagWithId t str content =
  "<" ++ t ++ " " ++ htmlId str ++ ">" ++ content ++ "</" ++ t ++ ">"

tagWithStringInside :: String -> String -> String -> String
tagWithStringInside t str content =
  "<" ++ t ++ " " ++ str ++ ">" ++ content ++ "</" ++ t ++ ">"

boilerplate :: [ADT] -> String -> String
boilerplate content inputTitle =
  let title = fromMaybe "Converted HTML" (mfilter (not . null) (Just inputTitle))
   in "<!DOCTYPE html>\n"
        ++ "<html lang=\"en\">\n\n"
        ++ "<head>\n"
        ++ "    <meta charset=\"UTF-8\">\n"
        ++ "    <title>"
        ++ title
        ++ "</title>\n"
        ++ "</head>\n"
        ++ concatMap convertADTHTML content
        ++ "</html>\n"