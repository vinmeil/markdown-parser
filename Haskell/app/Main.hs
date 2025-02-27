{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Assignment (markdownParser)

import Assignment (convertADTHTMLWithBoilerplate, getTime, markdownParser)
import Control.Monad.Cont (MonadIO (liftIO))
import Data.Aeson (object, (.=))
import Data.Aeson.Key (fromString)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Instances (ParseResult (Result), parse)
import Web.Scotty (ActionM, body, json, post, scotty)

getResult :: ParseResult a -> (a -> String) -> String
getResult (Result _ a) f = f a
getResult _ _ = ""

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
  json $ object [fromString key .= (pack value :: Text) | (key, value) <- pairs]

main :: IO ()
main = scotty 3000 $ do
  post "/api/convertMD" $ do
    requestBody <- body
    -- Convert the raw request body from ByteString to Text
    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = unpack requestBodyText

        -- Split the string into title and markdown
        (md, title) = break (== '💀') str
        newTitle = drop 1 title
        -- Parse the Markdown string using 'markdownParser' and apply 'convertAllHTML'
        -- converted_html = getResult (parse markdownParser md) convertADTHTML
        converted_html = getResult (parse markdownParser md)
                                   (convertADTHTMLWithBoilerplate newTitle)

    -- Respond with the converted HTML as JSON
    jsonResponse [("html", converted_html)]

  post "/api/saveHTML" $ do
    -- gets the request body
    requestBody <- body
    -- Convert the raw request body from ByteString to Text
    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = unpack requestBodyText

    -- save the contents of str into a file where the file name is the current time, using getTime
    time <- liftIO getTime
    liftIO $ writeFile (time ++ ".html") str
    jsonResponse [("success", "true")]