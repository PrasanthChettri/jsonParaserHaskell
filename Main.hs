module Main where

data JsonValue = JsonNull
                | JsonBoolean Bool
                | JsonNumber Integer  
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject [(String, JsonValue)]
                deriving (Show, Eq)

newtype Parser a = Parser {  
                      runParser :: String -> Maybe (String, a)
                        -- Nothing if fails, Just (String,a) if sucess
                }

--NULL PARSER 
jsonNull :: Parser JsonValue
jsonNull  = undefined

charP :: Char -> Parser Char
charP x = Parser f
            where
                f (y:ys) 
                    | y == x  = Just (ys, x)
                    | otherwise  = Nothing 
                f [] = Nothing

stringP :: String -> Parser String 
stringP = undefined

jsonValue :: Parser JsonValue
jsonValue = undefined

parser = undefined

main :: IO()
main = undefined
