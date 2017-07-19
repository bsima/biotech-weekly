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


import qualified Data.List as List
import Data.Maybe (catMaybes)
import Flow
import qualified Network.HTTP as Http
import Text.HTML.TagSoup


main :: IO ()
main = do
  src <- openUrl (baseUri ++ "/archive")
  let parsed = parseTags src
  let hrefs = getIndexLinks parsed
  archives <- mapM downloadIssue hrefs
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


getIndexLinks :: [Tag String] -> [String]
getIndexLinks tags =
  sections (~/= "<a>") tags
  |> map (filter isTagOpen)
  |> map safeHead
  |> catMaybes
  |> map (fromAttrib "href")
  |> filter (/= "")
  |> filter (List.isPrefixOf "/20")
  |> map (baseUri ++)


getIssueContent :: [Tag String] -> [Tag String]
getIssueContent html =
  html
  |> dropWhile (~/= "<center>")
  |> takeWhile (~/= "</center>")


downloadIssue :: String -> IO [Tag String]
downloadIssue url =
  do src <- openUrl url
     let content = getIssueContent <| parseTags src
     return content


getCanonicalUrl :: String -> String
getCanonicalUrl _ =
  undefined
