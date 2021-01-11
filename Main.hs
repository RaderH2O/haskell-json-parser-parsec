module Main where
import Text.Parsec
import Data.Char

data JSON = JNull
          | JBool Bool
          | JNum Double
          | JString String
          | JObj [(String, JSON)]
          | JArr [JSON] deriving Show

jnull :: Parsec String st JSON
jnull = f <$> string "null"
  where f x
          | x == "null" = JNull

jbool :: Parsec String st JSON
jbool = f <$> (string "false" <|> string "true")
  where f x
          | x == "true" = JBool True
          | x == "false" = JBool False

jnum :: Parsec String st JSON
jnum = f <$> (many (char '1' <|> char '2' <|> char '3' <|> char '4'
                    <|> char '5' <|> char '6' <|> char '7' <|> char '8'
                    <|> char '9' <|> char '0' <|> char '.'))
  where f x = JNum $ read x

stringLiteral :: Parsec String st String
stringLiteral = char '"' *> (many $ noneOf ['"']) <* char '"'

jstring :: Parsec String st JSON
jstring = JString <$> stringLiteral

ws :: Parsec String st String
ws = many $ char ' '

jobj :: Parsec String st JSON
jobj = JObj <$> (char '{' *> ws *>
       (sepBy (pairs) (char ',')) <* ws <* char '}')
  where pairs = (\x _ y -> (x, y)) <$> stringLiteral <*> (ws *> char ':' <* ws) <*> jvalue

jarr :: Parsec String st JSON
jarr = JArr <$> (char '[' *> (sepBy (jvalue) (ws *> char ',' <* ws)) <* char ']')

jvalue = jnull <|> jbool <|> jobj <|> jarr <|> jnum

main :: IO ()
main = undefined
