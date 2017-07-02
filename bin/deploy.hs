#!/usr/bin/env stack
{-
  stack
  --resolver lts-8.0
  script
  --package directory
  --package filepath
  --package process
-}

import qualified Control.Monad      as Monad
import qualified System.Directory   as Directory
import qualified System.Environment as Environment
import qualified System.FilePath    as FilePath
import qualified System.Process     as Process

main :: IO ()
main = do
  token <- Environment.getEnv "GITHUB_TOKEN"
  branch <- Environment.getEnv "TRAVIS_BRANCH"
  commit <- Environment.getEnv "TRAVIS_COMMIT"
  isPullRequest <- Environment.getEnv "TRAVIS_PULL_REQUEST"

  Monad.guard (branch == "v2")
  Monad.guard (isPullRequest == "false")

  Directory.setCurrentDirectory (FilePath.joinPath ["_hakyll_", "site"])
  writeFile "CNAME" "biotechweekly.com"
  let git = Process.callProcess "git"

  git ["init"]
  git ["add", "."]
  git ["config", "--global", "user.email", "ben@bsima.me"]
  git ["config", "--global", "user.name", "Ben Sima"]
  git ["commit", "--author", "Biotech Weekly <info@biotechweekly.com>", "--message", "Automatic deploy of " ++ commit]
  git ["remote", "add", "origin", "https://" ++ token ++ "@github.com/bsima/biotech-weekly.git"]
  git ["push", "--force", "--quiet", "origin", "master"]

