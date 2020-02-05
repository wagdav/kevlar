module Kevlar.GitHub.ParseUrl
  ( parseUrl,
  )
where

import Text.Parsec
import Text.Parsec.String

type Owner = String

type Name = String

type Project = (Owner, Name)

parseUrl :: String -> Maybe Project
parseUrl input = case parse url "" input of
  Left err -> Nothing
  Right p -> Just p

url :: Parser Project
url = do
  string "git@github.com:" <|> string "https://github.com/"
  owner <- many1 alphaNum
  char '/'
  repo <- many1 alphaNum
  optional $ string ".git"
  eof
  return (owner, repo)
