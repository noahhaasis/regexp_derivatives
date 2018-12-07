module Regex (Language (..), match, matchLanguage) where

import Text.ParserCombinators.Parsec
import Data.List.Split

data Language
  = Singleton Char
  | Alt Language Language
  | Cat Language Language
  | Rep Language
  | Eps
  | Empty
  deriving (Show)

isAcceptingLang :: Language -> Bool
isAcceptingLang Empty         = False
isAcceptingLang (Singleton _) = False
isAcceptingLang Eps           = True
isAcceptingLang (Rep _)       = True
isAcceptingLang (Alt l1 l2)   = isAcceptingLang l1 || isAcceptingLang l2
isAcceptingLang (Cat l1 l2)   = isAcceptingLang l1 && isAcceptingLang l2

derive :: Char -> Language -> Language
derive _ Empty         = Empty
derive _ Eps           = Empty
derive c (Singleton d) = if c == d then Eps else Empty
derive c (Cat l1 l2)   =
  if isAcceptingLang l1 then Alt (Cat  (derive c l1) l2) (derive c l2)
  else Cat (derive c l1) l2
derive c (Alt l1 l2)   = Alt (derive c l1) (derive c l2)
derive c (Rep l)       = Cat (derive c l) (Rep l)

matchLanguage :: Language -> String -> Bool
matchLanguage l ""     =  isAcceptingLang l
matchLanguage l (x:xs) = matchLanguage (derive x l) xs

concatenation :: Parser Language
concatenation = conjunct . toSingletonList <$> many (noneOf ['|'])
  where conjunct = foldr Cat Eps
        toSingletonList = map Singleton

regularExpression :: Parser Language
regularExpression = disjunct <$> (concatenation `sepBy1` char '|')
  where disjunct = foldr Alt Empty

strToLanguage :: String -> Language
strToLanguage p = case parse regularExpression "" p of
  (Right l) -> l
  _         -> error "Impossible: There is no unacceptable regular expression"

match :: String -> String -> Bool
match p = matchLanguage (strToLanguage p)
