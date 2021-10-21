module THUtils
  ( dropCamel
  , dropCamelLower
  , camelWords
  , jsonOptions
  ) where

import Data.Maybe
import Data.Char
import Data.Tuple
import Data.List
import Data.Aeson.TH

import Utils

type CamelSplitter = String -> Maybe (String, String)

unitarySplitter :: CamelSplitter
unitarySplitter str@[_] = Just (str, [])
unitarySplitter _ = Nothing

splitSimpleWord :: CamelSplitter
splitSimpleWord (a:firstRest@(b:secondRest))
  | not . isLetter $ a = Nothing
  | isLower a = let (restWord, tail) = span isLower firstRest
                in Just (a:restWord, tail)
  | isLower b = let (restWord, tail) = span isLower secondRest
                in Just (a:b:restWord, tail)
  | otherwise = Nothing
splitSimpleWord _ = Nothing

splitSymbolWord :: CamelSplitter
splitSymbolWord (a:rest)
  | not . isLetter $ a = let (restWord, tail) = span (not . isLower) rest
                         in Just $ (a:restWord, tail)
  | otherwise = Nothing
splitSymbolWord _ = Nothing

splitCapitalWord :: CamelSplitter
splitCapitalWord str@[a]
  | isUpper a = Just (str, [])
  | otherwise = Nothing
splitCapitalWord (a:firstRest@(b:secondRest))
  | not . isUpper $ a = Nothing
  | not . isLetter $ b = Just ([a], firstRest)
  | not . isUpper $ b = Nothing
  | otherwise = let (restWord, tail) = fromMaybe ([], firstRest) $ splitCapitalWord firstRest
                in Just (a:restWord, tail)
splitCapitalWord _ = Nothing

splitRules :: [String -> Maybe (String, String)]
splitRules =
  [ unitarySplitter
  , splitSimpleWord
  , splitSymbolWord
  , splitCapitalWord
  ]

splitWord :: String -> (String, String)
splitWord str = fromMaybe (str, []) $ safeHead $ do
  rule <- splitRules
  maybeToList $ rule str

camelWords :: String -> [String]
camelWords [] = []
camelWords str = headWord : (camelWords rest)
  where
    (headWord, rest) = splitWord str

joinWords :: [String] -> String
joinWords words = foldr (++) "" words

dropCamel :: Int -> String -> String
dropCamel count str = joinWords $ drop count $ camelWords str

lowerFirst [] = []
lowerFirst (word:rest) = (map toLower word):rest

dropCamelLower :: Int -> String -> String
dropCamelLower count str = joinWords $ lowerFirst $ drop count $ camelWords str

jsonOptions = defaultOptions
  { omitNothingFields = True
  }
