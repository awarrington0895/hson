module Main where

import Control.Applicative
import Data.Char

-- import Text.ParserCombinators.ReadPrec (reset)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- NOTE: No good error messages
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser parseFn) = Parser $ \input -> do
    (rest, x) <- parseFn input
    Just (rest, f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (rest, f) <- p1 input
    (rest', a) <- p2 rest
    Just (rest', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs
    then Nothing
    else Just (input', xs)

spanP :: (Char -> Bool) -> Parser String
spanP predicate = Parser $ \input ->
  let (token, rest) = span predicate input
   in Just (rest, token)

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = toBool <$> (stringP "true" <|> stringP "false")
  where
    toBool "true" = JsonBool True
    toBool "false" = JsonBool False

jsonNumber :: Parser JsonValue
jsonNumber = toNumber <$> notNull (spanP isDigit)
  where
    toNumber digits = JsonNumber $ read digits

-- NOTE: We do not support escaped quotes
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject
    <$> ( charP '{'
            *> ws
            *> sepBy pairSeparator pair
            <* ws
            <* charP '}'
        )
  where
    pairSeparator = ws *> charP ',' <* ws
    keyValueSeparator = ws *> charP ':' <* ws
    pair = (\key _ value -> (key, value)) <$> stringLiteral <*> keyValueSeparator <*> jsonValue

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
