{-# LANGUAGE OverloadedStrings #-}

module GUIParser where

import GUIConstants
import GUIDataTypes

import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import Data.Char

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

sampleText = readFile "assets/testFile.html"

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

{- Takes a list of html variables, replaces them in the html, and parses it-}
parseHTML :: Parser [HTML]
parseHTML = filter (EmptySpace /=) <$> many parseTag

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

-- There probably is a fancy way to create this function without the need for an argument
skipCh :: a -> Parser a
skipCh a = Parser (\s -> let rest = safeTail s in
                           [(a, rest)])

parseCharacters :: Parser String
parseCharacters = many $ satisfy (\c -> isAlpha c || isDigit c)

boolToMaybe :: (a -> Bool) -> (a -> Maybe a)
boolToMaybe f s = if f s then Just s else Nothing

attempt :: (a -> Maybe b) -> [a] -> Maybe b
attempt _ [] = Nothing
attempt f (x:xs) = case f x of
                  Just a -> Just a
                  _ -> attempt f xs

maybeRead :: Maybe String -> Maybe FontSize
maybeRead a = case a of
                Nothing -> Nothing
                Just x -> Just (read x)

--TODO: This function should be more general to make it easier to create other parameters
sortParameters :: [String] -> (Color, FontSize)
sortParameters [] = (defaultColor, defaultFontSize)
sortParameters strLst = let colorList :: [(String, Color)]
                            colorList = [("red", red), ("white", white), ("blue", blue), ("black", black), ("green", green)]
                            color = fromMaybe defaultColor $ attempt (`lookup` colorList) strLst
                            font = fromMaybe defaultFontSize $ maybeRead $ attempt (boolToMaybe $ all isDigit) strLst in
                          (color, font)

parseParagraphParameters :: Parser String
parseParagraphParameters = (\_ _ x -> x) <$> skipSpaces <*> (string "color=" <|> string "size=") <*> parseCharacters

parseParagraph :: Parser HTML
parseParagraph = (\_ params _ text -> let (color, fontSize) = sortParameters params in
                                        Paragraph text color fontSize)
          <$> string "<p" <*> many parseParagraphParameters <*> parseUntil2 ">" <*> parseUntil2 "</p>"


parseBreakParameters :: Parser String
parseBreakParameters = (\_ _ params -> params) <$> skipSpaces <*> string "size=" <*> parseCharacters

replaceWord :: String -> String -> String -> String
replaceWord str oldWord newWord = unwords $ fst $ foldl (\(newStrWords, foundIt) currentStr ->
                                                          if currentStr == oldWord && not foundIt
                                                          then (newStrWords ++ [newWord], True)
                                                          else (newStrWords ++ [currentStr], foundIt)) ([], False) $ words str
{-
{- The Packs are ugly i known sue me :) -}
loadVariables :: [(String, String)] -> String -> String
loadVariables [] html = html
loadVariables ((varName, varValue):rest) html = let newHTML = T.replace (T.pack varName) (T.pack varValue) (T.pack html) in
  loadVariables rest (T.unpack newHTML)
-}
loadVariable :: HTMLVar -> String -> String
loadVariable (varName, varValue) html = T.unpack $ T.replace (T.pack varName) (T.pack varValue) (T.pack html)

parseBreak :: Parser HTML
parseBreak = (\_ param _ -> let breakSize = fromMaybe defaultBreakSize $ maybeRead $ attempt (boolToMaybe $ all isDigit) [param] in
                 Break breakSize) <$> string "<br" <*> parseBreakParameters <*> parseUntil2 ">"

parseButton :: Parser HTML
parseButton = replace HButton "<button>"

replace :: a -> String -> Parser a
replace a str = Parser (\s -> [(a, rest) | (_, rest) <- runParser (string str) s])

satisfy :: (Char -> Bool) -> Parser Char
satisfy op = Parser (\s -> [(res, rest) | let (res, rest) = (head s, tail s), op res])

parseWord :: Parser String
parseWord = many $ satisfy isAlpha

-- Same as parseUntil but consumes the stopStr from the rest e.g:
-- runParser (parseUntil2 "STOP") "test STOP bro" = [("test ", " bro")]
parseUntil2 :: String -> Parser String
parseUntil2 stopStr = Parser (\s -> [(res, drop (length stopStr) rest) | (res, rest) <- runParser (parseUntil stopStr) s])

parseUntil :: String -> Parser String
parseUntil stopStr = Parser (\s -> let (res, rest) = go s "" in
                                      [(res, rest)]) where
  stopLen = length stopStr
  go [] result = (result, "")
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
