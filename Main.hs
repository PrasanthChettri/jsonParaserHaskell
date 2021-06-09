module Main where

import           Control.Applicative
import           Data.Char
import           Numeric
import           System.Exit


data JsonValue 
    = JsonNull
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
--proving functor
instance Functor Parser where 
    fmap f (Parser p) = Parser $ \input -> do 
                                (input' , x) <- p input
                                return (input', f x)

--proving applicative
instance Applicative Parser where 
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) =
                            Parser $ \input -> do 
                                    (input' , f) <- p1 input
                                    (input'' , a) <- p2 input'
                                    Just (input'' , f a)

--proving alternative
instance Alternative Parser where 
    empty = Parser $ \_ -> Nothing 
    (Parser p1) <|> (Parser p2) =  
            Parser $ \input -> p1 input <|> p2 input

--SPAN DIGIT
spanP :: (Char  -> Bool) -> Parser String
spanP f = Parser $ \input -> 
        let (token, rest) = span f input
            in Just (rest, token)


charP :: Char -> Parser Char
charP x = Parser f
            where
                f (y:ys) 
                    | y == x  = Just (ys, x)
                    | otherwise  = Nothing 
                f [] = Nothing



notNull :: Parser [a] -> Parser[a]
notNull (Parser p) = 
    Parser $ \input -> do 
                (input', xs) <- p input
                if null xs 
                        then Nothing
                else 
                        Just (input' , xs)

stringP :: String -> Parser String 
stringP = sequenceA . map charP

--NULL PARSER 
jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

--BOOL PARSER 
jsonBool :: Parser JsonValue 
jsonBool = f <$> (stringP "true" <|> stringP "false")
                where f "true" = JsonBoolean True
                      f "false" = JsonBoolean False
                      --monads should take care, should not happen
                      f _ = undefined
--NUNBER PARSER
jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
                where f ds = JsonNumber $ read ds

jsonValue :: Parser JsonValue
jsonValue =  jsonString <|> jsonNull <|> jsonBool <|> jsonNumber <|> jsonArray

stringLiteral :: Parser String
stringLiteral = spanP (/= '"')

jsonString :: Parser JsonValue 
jsonString =  JsonString <$> ( charP '"' *> stringLiteral <* charP '"' ) 

ws :: Parser String 
ws = spanP isSpace 

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element =  (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> ( charP '[' *> ws *>
                            elements 
                            <* ws <* charP ']' ) 
                where
                    elements = sepBy (ws *> charP  ',' <* ws) jsonValue

main :: IO()
main = undefined
