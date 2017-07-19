#!/usr/bin/env stack
{-
  stack
  --resolver lts-8.23
  script
  --package directory
  --package filepath
  --package process
  --package HTTP
  --package tagsoup
  --package flow
-}

-- | One-off script to download and cleanup all the newsletter issues
-- on biotechweekly.com

import qualified Data.List as List
import Data.Maybe (catMaybes)
import Flow
import qualified Network.HTTP as Http
import Text.HTML.TagSoup


-- | Convenience type
type Html =
  [Tag String]


main :: IO ()
main =
  do src <- openUrl (baseUri ++ "/archive")
     let parsed = parseTags src
     let hrefs = selectIndexLinks parsed
     archives <- mapM getIssue hrefs
     putStrLn $ show $ length archives


baseUri :: String
baseUri =
  "http://www.biotechweekly.com"


outDir :: String
outDir =
  "content/archive"


openUrl :: String -> IO String
openUrl x =
  Http.getResponseBody =<< Http.simpleHTTP (Http.getRequest x)


safeHead :: [a] -> Maybe a
safeHead ls =
  case ls of
    (x:_) -> Just x
    []    -> Nothing


-- | Isolates only the links to newsletter archive pages and returns a
-- list of fully-qualified URLs to each of the archived newsletter issues.
selectIndexLinks :: Html -> [String]
selectIndexLinks tags =
  sections (~/= "<a>") tags
  |> map (filter isTagOpen)
  |> map safeHead
  |> catMaybes
  |> map (fromAttrib "href")
  |> filter (/= "")
  |> filter (List.isPrefixOf "/20")
  |> map (baseUri ++)


-- | Selects only the content of the newsletter
selectIssueContent :: Html -> Html
selectIssueContent html =
  html
  |> dropWhile (~/= "<center>")
  |> removeImages
  |> takeWhile (~/= "</center>")


-- | Retrieves the newsletter content from the `url`
getIssue :: String -> IO Html
getIssue url =
  do src <- openUrl url
     let content = selectIssueContent <| parseTags src
     return content


-- | Gets the articles linked to from each newsletter
selectMaskedLinks :: Html -> Html
selectMaskedLinks html =
  html
  |> sections (~/= "<a>")
  |> concat


-- | Simply strips out images from the Html
removeImages :: Html -> Html
removeImages html =
  takeWhile (~/= "<img>") html


-- | Makes an HTTP request and then gets the `canonical` link from the
-- `head` of the document.
getCanonicalUrl :: String -> IO String
getCanonicalUrl url =
  do html <- openUrl url
     let can = fromAttrib "href"
               $ head
               $ dropWhile (~/= "<link ref=canonical>")
               $ parseTags html
     return can
