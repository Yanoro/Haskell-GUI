{-# LANGUAGE OverloadedStrings #-}
module Parser where

import DataTypes -- Remove later; placed due to color variables used in parse parameters

import Control.Applicative
import Data.Char
import DataTypes

instance Functor Parser where
  fmap f (Parser p) = Parser (\s -> [(f res, rest) | (res, rest) <- p s])

instance Applicative Parser where
  pure a = Parser (\s -> [(a, s)])
  pa <*> pb = Parser (\s -> [ (ra rb, rstB) | (ra, rstA) <- runParser pa s, (rb , rstB) <- runParser pb rstA])

instance Alternative Parser where
  empty = Parser $ const []
  pa <|> pb = Parser $ \s -> runParser pa s ++ runParser pb s

newtype Parser a = Parser {
  runParser :: String -> [(a, String)]
}

sampleText = "<p> This is a sample text! </p>\
             \<br>\
             \<p> And this is another one! </p>"

{-
<p> This is another sample text! </p>
<button>
<img src="cute.cat">
<button>

----------
This should be enough for our needs, however we also need to allow
the window to interact with us, for instance with the button tag.
-}

skipSpaces :: Parser String
skipSpaces = many $ char ' '

parseTag :: Parser HTML
parseTag = parseParagraph <|> parseBreak <|> parseButton <|> parseEmptySpace where
  parseEmptySpace = replace EmptySpace " " <|> replace EmptySpace "\n"

failParser :: Parser a -> Parser a -> Parser a
failParser pa pb = Parser (\s -> let res = runParser pa s in
                                   case res of
                                     [] -> runParser pb s
                                     _  -> res)

parseHTML :: Parser [HTML]
parseHTML = filter (EmptySpace /=) <$> many parseTag

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) | null xs = []
                | otherwise = xs

-- There probably is a fancy way to create this function without the need for an argument
skipCh :: a -> Parser a
skipCh a = Parser (\s -> let rest = safeTail s in
                           [(a, rest)])

parseCharacters :: Parser String
parseCharacters = many $ satisfy (\c -> isAlpha c || isDigit c)

-- <p color=green size=12>
parseTagParameters :: Parser String
parseTagParameters = (\_ x -> x) <$> (string "color=" <|> string "size=") <*> parseCharacters

-- TODO: Set default Color and fontSize
sortParameters :: String -> String -> (Color, FontSize)
sortParameters str1 str2 = let strlst = [str1, str2] in
                             if not $ any (all isDigit) strlst && any (all isAlpha) strlst
                             then (white, 12)
                             else let color = case head $ filter (all isAlpha) strlst of
                                                "green" -> green
                                                "black" -> black
                                                _       -> white
                                      fontSize = read $ head $ filter (all isDigit) strlst in
                                    (color, fontSize)

parseParagraph :: Parser HTML
parseParagraph = (\_ _ param1 _ param2 _ text -> let (color, fontSize) = sortParameters param1 param2 in
                                                   Paragraph text color fontSize)
          <$> startTag <*> skipSpaces <*> parseTagParameters <*> skipSpaces <*> parseTagParameters <*> parseUntil2 ">" <*> parseUntil2 "</p>"
  where startTag = string "<p"

parseBreak :: Parser HTML
parseBreak = replace Break "<br>"

parseButton :: Parser HTML
parseButton = replace HButton "<button>"

replace :: a -> String -> Parser a
replace a str = Parser (\s -> [(a, rest) | (_, rest) <- runParser (string str) s])

satisfy :: (Char -> Bool) -> Parser Char
satisfy op = Parser (\s -> [(res, rest) | let (res, rest) = (head s, tail s), op res])

parseWord :: Parser String
parseWord = many $ satisfy isAlpha

-- Same as parseUntil but removes the stopStr from the rest e.g:
-- runParser (parseUntil2 "STOP") "test STOP bro" = [("test ", " bro")]
parseUntil2 :: String -> Parser String
parseUntil2 stopStr = Parser (\s -> [(res, drop (length stopStr) rest) | (res, rest) <- runParser (parseUntil stopStr) s])

parseUntil :: String -> Parser String
parseUntil stopStr = Parser (\s -> let (res, rest) = go s "" in
                                      [(res, rest)]) where
  stopLen = length stopStr
  go str result = if take stopLen str == stopStr
                  then (result, str)
                  else go (tail str) (result ++ [head str])

split :: String -> String -> String
split str stopStr = unwords $ go $ words str where
  go [] = []
  go (x:xs) = if x == stopStr
              then []
              else x : go xs

char :: Char -> Parser Char
char ch = satisfy (==ch)

string :: String -> Parser String
string str = Parser (\s -> [(res, rest) | let (res, rest) = splitAt (length str) s, res == str])
