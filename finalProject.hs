import Text.Parsec
import Text.Parsec.String (Parser)

data Fact = F String Var 
   deriving (Show, Eq)
data Var = V String | V' Var Var
   deriving (Show, Eq)

parseFact :: Parser Fact
parseFact = do
   fact <- many1 letter
   char '('
   variables <- parseVar
   char ')'
   char '.'
   return (F fact variables)

parseVar :: Parser Var
parseVar = do {var <- many1 (noneOf ",)"); return (V var)}

parseFactFinal :: String -> Either ParseError Fact
parseFactFinal input = parse parseFact "" input


