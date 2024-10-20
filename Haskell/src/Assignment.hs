module Assignment (markdownParser, convertADTHTML, convertADTHTMLWithBoilerplate, getTime) where

import Control.Applicative
import Control.Monad (guard, mfilter)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (ParseError (..), ParseResult (..), Parser (..))
import Parser

-- BNF
-- <Number>          ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
-- <PositiveInt>     ::= <Number> | <Number> <PositiveInt>
-- <Char>            ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "|" | " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | ">" | "=" | "<" | "?" | "@" | "[" | "\" | "]" | "^" | "_" | "`" | "{" | "}" | "~"
-- <JustText>        ::= <Char> | <Char> <JustText>
-- <Whitespace>      ::= ' ' | '\t' | '\n' | '\r' | '\f' | '\v'
-- <Italic>          ::= '_' <TextAndModifier> '_'
-- <Bold>            ::= '**' <TextAndModifier> '**'
-- <Strikethrough>   ::= '~~' <TextAndModifier> '~~'
-- <Link>            ::= '[' <TextAndModifier> ']' '(' <JustText> ')'
-- <InlineCode>      ::= '`' <TextAndModifier> '`'
-- <Footnote>        ::= '[^' <PositiveInt> ']'
-- <Modifiers>       ::= <Italic> | <Bold> | <Strikethrough> |
--                       <Link> | <InlineCode> | <Footnote>
-- <Image>           ::= '![' <JustText> ']'
--                       '(' <JustText> <Whitespace> '"' <JustText> '"' ')'
-- <FootnoteRef>     ::= <Footnote> ':' <JustText>
-- <TextAndModifier> ::= <JustText>+ | <Modifiers>+ | <JustText>+ <Modifiers>+
-- <Paragraph>       ::= <TextAndModifier>
-- <Heading1>        ::= '#' <Whitespace> <TextAndModifier> '\n'
-- <Heading2>        ::= '##' <Whitespace> <TextAndModifier> '\n'
-- <Heading3>        ::= '###' <Whitespace> <TextAndModifier> '\n'
-- <Heading4>        ::= '####' <Whitespace> <TextAndModifier> '\n'
-- <Heading5>        ::= '#####' <Whitespace> <TextAndModifier> '\n'
-- <Heading6>        ::= '######' <Whitespace> <TextAndModifier> '\n'
-- <AltHeading1>     ::= <TextAndModifier> '\n' '==' {'='}
-- <AltHeading2>     ::= <TextAndModifier> '\n' '--' {'-'}
-- <Heading>         ::= <Heading1> | <Heading2> | <Heading3> | <Heading4> |
--                       <Heading5> | <Heading6> | <AltHeading1> | <AltHeading2>
-- <Blockquote>      ::= '>' <Paragraph>
-- <Code>            ::= '```' [JustText] '\n' <JustText> '\n```'
-- <ListItem>        ::= <TextAndModifier> | <OrderedList>
-- <OrderedList>     ::= <ListItem> | <ListItem> <OrderedList>
-- <Table>           ::= <TableHeader> <TableRow>+
-- <TableHeader>     ::= <TableHeaderCell>+
-- <TableRow>        ::= <TableHeaderCell>+ | <TableRowCell>+
-- <TableHeaderCell> ::= <TextAndModifier>
-- <TableRowCell>    ::= <TextAndModifier>
-- <Markdown>        ::= <Paragraph> | <Heading> | <Blockquote> | <Code> | <OrderedList> | <Table>

data ADT
  = Empty
  | JustText String
  | Newline
  | Paragraph [ADT]
  | Italic [ADT]
  | Bold [ADT]
  | Strikethrough [ADT]
  | Link ([ADT], String)
  | InlineCode [ADT]
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

-- modifier prefixes to stop free text parser
modifierPrefixes :: [String]
modifierPrefixes = ["|", "_", "**", "~~", "[", "`", "[^", "![", "\n", ">"]

-- == HELPER FUNCTIONS == --

-- checks for a positive integer without leading space
positiveInt :: Parser Int
positiveInt = do
  _   <- isParserSucceed notSpace -- ensure no leading space
  num <- int
  _   <- guard (num > 0) <|> unexpectedStringParser "Number must be positive"
  return num

-- checks if a string starts with a certain prefix
startsWith :: String -> Parser ()
startsWith = isParserSucceed . string

-- parses a string until a certain string is found
parseUntil :: String -> Parser String
parseUntil str = f (0 :: Int)
  where
    -- use recursion to concat letter by letter until it finds the string
    f 0   = (:) <$> char <*> f 1 -- ensure has at least 1 letter before checking
    f len = ( isParserSucceed (string str) $> "" ) <|> 
            ( (:) <$> char <*> f (len + 1) )

-- parses until a modifier prefix is found
parseUntilModifier :: Parser String
parseUntilModifier = do
  c    <- char -- consume first character because modifier parsers already failed
  rest <- (asum (map startsWith modifierPrefixes) $> "") <|> parseUntilModifier
  return (c : rest)

-- parses at most N of a certain character, errors if theres more
parseAtMost :: Int -> Char -> Parser String
parseAtMost lim ch = do
  str <- some (is ch)
  _   <- guard (length str <= lim) <|>
         unexpectedStringParser "Parsed too many characters"
  return str

-- parses at least N of a certain character, errors if theres less
parseAtLeast :: Int -> Char -> Parser String
parseAtLeast lim ch = do
  str <- some (is ch)
  _   <- guard (length str >= lim) <|> 
         unexpectedStringParser "Not enough characters"
  return str

-- parses characters until end of file
parseUntilEof :: Parser String
parseUntilEof = some char

-- parses character by character until an inline space is found
parseUntilInlineSpace :: Parser String
parseUntilInlineSpace =  do
  whitespace <- inlineSpace
  if not (null whitespace)
    then pure ""
    else do (:) <$> char <*> parseUntilInlineSpace

-- parses only text until a newline is found
parseUntilNewline :: Parser String
parseUntilNewline = parseUntil "\n"

-- parses both normal text and modifiers until the delimiter is found
parseAllUntil :: String -> Parser [ADT]
parseAllUntil delim =
  some ( 
    parseModifiers <|>
    ( isParserSucceed (isNotString delim *> isNotString "\n") *> justText )
  )

-- parses just text between 2 strings
getStringBetween :: String -> String -> Parser String
getStringBetween st1 st2 = string st1 *> parseUntil st2 <* string st2

-- parses adts between 2 strings
getADTBetween :: String -> String -> Parser [ADT]
getADTBetween st1 st2 = string st1 *> parseAllUntil st2 <* string st2

-- checks if a parser returns an error or not without consuming input
isParserSucceed :: Parser a -> Parser ()
isParserSucceed (Parser p) = Parser $ \input ->
  case p input of
    Result _ _ -> Result input ()
    Error _    -> Error UnexpectedEof

parseModifierAndTextUntilNewline :: Parser [ADT]
parseModifierAndTextUntilNewline = parseAllUntil "\n"

-- == PARSERS == --

-- html text parsers --

-- parses a newline character into the Newlineadt
newline :: Parser ADT
newline = is '\n' $> Newline

-- parses non modifier text into JustText adt
justText :: Parser ADT
justText = JustText <$> ( parseUntilModifier <|> 
                          parseUntilNewline <|> -- if modifier prefix not found
                          parseUntilEof ) -- if newline not found

-- parses string into italic adt
italic :: Parser ADT
italic = Italic <$> getADTBetween "_" "_"

-- parses string into bold adt
bold :: Parser ADT
bold = Bold <$> getADTBetween "**" "**"

-- parses string into strikethrough adt
strikethrough :: Parser ADT
strikethrough = Strikethrough <$> getADTBetween "~~" "~~"

-- parses string into link adt
link :: Parser ADT
link = do
  text <- getADTBetween "[" "]" <* inlineSpace
  url  <- getStringBetween "(" ")"
  return $ Link (text, url)

-- parses string into inlinecode adt
inlinecode :: Parser ADT
inlinecode = InlineCode <$> getADTBetween "`" "`"

-- gets the number inside of a footnote
getFootnoteNumber :: Parser Int
getFootnoteNumber = do
  _   <- string "[^"
  num <- positiveInt
  _   <- string "]"
  return num

-- parses string into footnote adt
footnote :: Parser ADT
footnote = Footnote <$> getFootnoteNumber

-- NON MODIFIER PARSERS

-- parses string into Image adt
image :: Parser ADT
image = do
  alt   <- getStringBetween "![" "]"
  _     <- inlineSpace
  url   <- string "(" *> parseUntilInlineSpace
  title <- getStringBetween "\"" "\"" <* string ")"
  return $ Image (alt, url, title)

-- parses string into footnoteRef adt
footnoteRef :: Parser ADT
footnoteRef = do
  _    <- inlineSpace
  num  <- getFootnoteNumber <* string ":" <* inlineSpace
  text <- (parseUntilNewline <|> parseUntilEof) <* optional (is '\n')
  return $ FootnoteRef (num, text)

-- helper function to parse headings that use '#' as the prefix
headingHashtag :: Parser ADT
headingHashtag = do
  level <- length <$> (parseAtMost 6 '#' <* inlineSpace1)
  text  <- parseModifierAndTextUntilNewline
  return $ Heading (level, text)

-- helper function to parse headings that use '=' or '-' as the identifier
altHeading :: Int -> Char -> Parser ADT
altHeading n c = do
  text <- parseModifierAndTextUntilNewline <* is '\n'
  _    <- parseAtLeast 2 c <* inlineSpace <* -- remove spaces after dashes
          ( (is '\n' <|> (eof $> ' ')) <|> -- check if remaining char is '\n' or eof
             unexpectedStringParser "Unexpected characters after heading" )
  return $ Heading (n, text)

-- helper function to parse the alternative version of h1
altHeading1 :: Parser ADT
altHeading1 = altHeading 1 '='

-- helper function to parse the alternative version of h2
altHeading2 :: Parser ADT
altHeading2 = altHeading 2 '-'

-- parses string into heading adt
heading :: Parser ADT
heading = inlineSpace *> (headingHashtag <|> altHeading1 <|> altHeading2)

-- parses string into blockquote adt
blockquote :: Parser ADT
blockquote = do
  quotes <- some $ do
    _    <- inlineSpace
    _    <- charTok '>'
    adts <- paragraph
    _    <- optional (is '\n')
    return adts
  return $ Blockquote quotes

-- Helper function for code block to recursively parse until closing code block
-- i would make this look nicer but i forgot how it works and i am not touching it
parseUntilClosingCode :: Parser String
parseUntilClosingCode =
  (parseUntil "```\n" <* string "```\n") <|>
  (parseUntil "\n```" <* string "\n```" <* eof) <|> -- if no newline at the end
  ((:) <$> char <*> parseUntilClosingCode) -- for cases where theres a ``` with text after it

-- parses string into code adt
code :: Parser ADT
code = do
  lang <- string "```\n" $> "" <|> getStringBetween "```" "\n"
  text <- parseUntilClosingCode
  return $ Code (lang, text)

-- helper function for ordered list to check for proper indentation
guardIndentation :: String -> Parser ()
guardIndentation indent = do
  -- make sure theres no other whitespace
  _          <- isParserSucceed (noneof "\t\r\f\v\n") <|> 
                unexpectedStringParser "Only expected spaces"
  whitespace <- many (is ' ')
  guard (whitespace == indent) <|> 
    unexpectedStringParser "Unexpected number of spaces"

-- helper function for ordered list to parse a line of an ordered list
parseListItemAux :: String -> Parser a -> Parser [ADT]
parseListItemAux indent parser = do
  _        <- guardIndentation indent
  _        <- parser -- parses the number at the front
  textADTs <- parseModifierAndTextUntilNewline <* optional (is '\n')
  subList  <- parseSublist (indent ++ "    ") <|> pure Empty
  let listItems = ListItem textADTs : [subList] -- concats the sublist to the list
  return listItems

-- helper function for ordered list to parse list items which calls an aux function
parseListItem :: Int -> String -> Parser [ADT]
parseListItem 1 indent = parseListItemAux indent (string "1. " <* inlineSpace)
parseListItem _ indent = parseListItemAux indent (positiveInt <* string ". " <* inlineSpace)

-- parses sublists in an ordered list based on indentation
parseSublist :: String -> Parser ADT
parseSublist indent = do
  firstItem <- parseListItem 1 indent
  rest      <- many (parseListItem 0 indent)
  return $ OrderedList (concat (firstItem : rest))

-- parses string into ordered list adt
-- note: hard to combine with above function because of extra indent parameter
orderedList :: Parser ADT
orderedList = do
  firstItem <- parseListItem 1 ""
  rest      <- many (parseListItem 0 "")
  return $ OrderedList (concat (firstItem : rest))

-- parses string into table adt
table :: Parser ADT
table = do
  (header, rowCount) <- parseHeader
  _    <- parseSeparator -- removes the separating line
  rows <- some (parseRow rowCount)
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

-- parses a row in a table
parseRow :: Int -> Parser ADT
parseRow cellCount = do 
  _     <- optional (is '\n')
  _     <- inlineSpace
  cells <- parseNCells cellCount
  _     <- string "|"
  return $ TableRow cells

-- parses N cells in a table
parseNCells :: Int -> Parser [ADT]
parseNCells 1 = (: []) <$> (TableRowCell <$> parseTableCell)
parseNCells n = do
  cell <- TableRowCell <$> parseTableCell -- parse cell
  rest <- parseNCells (n - 1) -- recursively parse the rest of the cells
  return (cell : rest)

parseTableCell :: Parser [ADT]
parseTableCell = do
  _ <- charTok '|' -- consume the first '|'
  adts <- some $ do -- get a list of adts
    _ <- isParserSucceed (isNot '|')
    parseModifiers <|> (JustText <$> (parseUntilModifier <|> parseUntil "|"))
  return (fmap trimADT adts) -- trim trailing whitespace

----------------------------------------------------------------------
-- got this function from github copilot, did not make this on my own.
-- trims trailing whitespace from a string
trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse
--
----------------------------------------------------------------------

-- trims trailing whitespace from an adt
trimADT :: ADT -> ADT
trimADT (JustText str) = JustText (trim str)
trimADT (Italic adts) = Italic (map trimADT adts)
trimADT (Bold adts) = Bold (map trimADT adts)
trimADT (Strikethrough adts) = Strikethrough (map trimADT adts)
trimADT (InlineCode adts) = InlineCode (map trimADT adts)
trimADT _ = Empty

-- parses text into adt
parseModifiers :: Parser ADT
parseModifiers = italic <|> bold <|> strikethrough <|> 
                 link <|> inlinecode <|> footnote

-- parses string into paragraph adt
paragraph :: Parser ADT
paragraph = Paragraph <$> parseModifierAndTextUntilNewline

-- parses the non modifier adts
parseNonModifiers :: Parser ADT
parseNonModifiers = footnoteRef <|> image <|> heading <|> 
                    blockquote <|> code <|> orderedList <|> table

-- == MAIN PARSER == --

-- parses the whole text into adts
parseText :: Parser [ADT]
parseText = allSpace *> many (newline <|> parseNonModifiers <|> paragraph)

-- wraps the whole result in a Markdown adt
markdownParser :: Parser ADT
markdownParser = Markdown <$> parseText

-- gets the current time in iso format
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

-- == HTML CONVERTER == --

----------------------------------------------------
-- COMMON PATTERN: (concatMap convertADTHTML adts)
-- this uses recursion to help convert nested adts since it concats and maps
-- everything inside the list to the function
--
-- hope i wont have to document every line where i use this to specify that it
-- uses recursion to convert nested adts (please dont deduct my marks)
----------------------------------------------------

-- helper function to parse footnote adt into a string
htmlFootnote :: Int -> String
htmlFootnote num = do
  let idAttr    = htmlId ("fn" ++ show num ++ "ref")
  let hrefAttr  = href ("#fn" ++ show num)
  let anchorTag = tagWithStringInside "a" (idAttr ++ " " ++ hrefAttr) (show num)
  tag "sup" anchorTag

-- helper function to parse image adt into a string
htmlImage :: String -> String -> String -> String
htmlImage alt url caption = do
  let src     = "src=\"" ++ url ++ "\""
  let altAttr = " alt=\"" ++ alt ++ "\""
  let title   = " title=\"" ++ caption ++ "\""
  altTag "img" (src ++ altAttr ++ title)

-- helper function to parse footnote reference adt into a string
htmlFootnoteRef :: Int -> String -> String
htmlFootnoteRef num content = tagWithId "p" ("fn" ++ show num) content ++ "\n"

-- helper function to parse paragraph adt into a string
htmlParagraph :: [ADT] -> String
htmlParagraph adts = tag "p" (concatMap convertADTHTML adts)

-- helper function to parse heading adt into a string
htmlHeading :: Int -> [ADT] -> String
htmlHeading n adts = tag ("h" ++ show n) (concatMap convertADTHTML adts) ++ "\n"

-- helper function to parse code adt into a string
htmlCodeWithLanguage :: String -> String -> String
htmlCodeWithLanguage lang content = do
  let codeTag = tagWithStringInside "code"
                ("class=\"language-" ++ lang ++ "\"")
                content
  let preTag  = tag "pre" codeTag
  preTag ++ "\n"

-- helper function to parse table adt into a string
htmlLink :: [ADT] -> String -> String
htmlLink url = tagWithHref "a" (concatMap convertADTHTML url) 

-- helper function to convert ADTs into HTML with the boilerplate
convertADTHTMLWithBoilerplate :: String -> ADT -> String
convertADTHTMLWithBoilerplate title (Markdown adts) = boilerplate adts title
convertADTHTMLWithBoilerplate _ _ = ""

-- helper function to add boilerplate code with default title
-- (this is for the test cases i dont actually need this function)
convertAll :: ADT -> String
convertAll (Markdown adts) = boilerplate adts ""
convertAll _ = ""

-- boilerplate code for the html which accepts a title to add to the html
boilerplate :: [ADT] -> String -> String
boilerplate content inputTitle =
  let title = fromMaybe "Test" -- defaults to "Test" if no title is given
                        (mfilter (not . null) (Just inputTitle))
  in "<!DOCTYPE html>\n"
        ++ "<html lang=\"en\">\n\n"
        ++ "<head>\n"
        ++ "    <meta charset=\"UTF-8\">\n"
        ++ "    <title>"
        ++ title
        ++ "</title>\n"
        ++ "</head>\n\n"
        ++ indentInside "body" content
        ++ "\n</html>\n"

-- main function to convert adts into html
convertADTHTML :: ADT -> String
convertADTHTML (Markdown adts) = convertAll (Markdown adts)
convertADTHTML (JustText str) = str
convertADTHTML Newline = ""
convertADTHTML Empty = ""
convertADTHTML (Italic adts) = tag "em" (concatMap convertADTHTML adts)
convertADTHTML (Bold adts) = tag "strong" (concatMap convertADTHTML adts)
convertADTHTML (Strikethrough adts) = tag "del" (concatMap convertADTHTML adts)
convertADTHTML (InlineCode adts) = tag "code" (concatMap convertADTHTML adts)
convertADTHTML (Link (text, url)) = htmlLink text url
convertADTHTML (Footnote num) = htmlFootnote num
convertADTHTML (Image (alt, url, caption)) = htmlImage alt url caption
convertADTHTML (FootnoteRef (num, content)) = htmlFootnoteRef num content
convertADTHTML (Paragraph adts) = htmlParagraph adts
convertADTHTML (Heading (n, adts)) = htmlHeading n adts
convertADTHTML (Blockquote adts) = indentInside "blockquote" adts
convertADTHTML (Code ("", content)) = tag "pre" (tag "code" content) ++ "\n"
convertADTHTML (Code (lang, content)) = htmlCodeWithLanguage lang content
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

-- helper function for an opening tag
openTag :: String -> String
openTag t = "<" ++ t ++ ">"

-- helper function for a closing tag
closeTag :: String -> String
closeTag t = "</" ++ t ++ ">"

-- helper function to filter out Empty adts
filterNonEmpty :: [ADT] -> [ADT]
filterNonEmpty = filter (\x -> x /= Empty && x /= Newline)

-- adds a specified tag around the content
tag :: String -> String -> String
tag t content = openTag t ++ content ++ closeTag t

-- adds an alt tag around content
altTag :: String -> String -> String
altTag t content = "<" ++ t ++ " " ++ content ++ ">"

-- creates a string prefixed by "href=""
href :: String -> String
href url = "href=\"" ++ url ++ "\""

-- creates a string prefixed by "id=""
htmlId :: String -> String
htmlId str = "id=\"" ++ str ++ "\""

-- creates a tag with a specified href
tagWithHref :: String -> String -> String -> String
tagWithHref t url content =
  "<" ++ t ++ " " ++ href url ++ ">" ++ content ++ "</" ++ t ++ ">"

-- creates a tag with a specified id
tagWithId :: String -> String -> String -> String
tagWithId t str content =
  "<" ++ t ++ " " ++ htmlId str ++ ">" ++ content ++ "</" ++ t ++ ">"

-- creates a tag with a specified string inside
tagWithStringInside :: String -> String -> String -> String
tagWithStringInside t str content =
  "<" ++ t ++ " " ++ str ++ ">" ++ content ++ "</" ++ t ++ ">"
