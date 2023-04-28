module Main where

data JsonValue
  = JsonNull
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- Note: No good error messages
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing

jsonNull :: Parser JsonValue
jsonNull = undefined

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined
