--What is a parser?
--Sandeep: Anything that read text which has structure
--A parser takes a string input and creates an output which assigns the
-- (a) string input to relevant types
-- (b) create a semantic structure (tree) out of the relevant input
-- parse "1234+156"
--What are the types involved here?
--Numerical 'types'
--Operator 'type' , some kind of a string
--parse "1234 + 156 "
--Second one has some kinds of string inputs in between the input, which we want to ignore
--parse "sum(1234, 156)"
--string input to convert to a function type, 
--a tuple input to be converted to a haskell tuple or a curried integers
--parse "sumlist([1,4,6,7])"
--function call
--numbers go to numbers,
--use the strings , and [ ]  to convert the string input into a list of integers/num type
--why does sequencing become important here?
--why is the type of a Parser?
--Parser a::String -> (a, String)"
--Question: Does this remind you of something?

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

--parse function takes a parse function f written as P f, and applies it to the string input given by inp
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

--item is a parse function, when used in conjunction with parse, is to use return the first element of the input string and rest of the string is stored in the second half of the tuple. 
item :: Parser Char
item = P (\inp -> case inp of
                      [] -> []
                      (x:xs) -> [(x,xs)])

--the following parses a string, and converts the first char into a integer value corresponding to its ascii value
item2:: Parser Int
item2 = P (\inp -> case inp of
                           [] -> []
                           (x:xs) -> [(ord x, xs)])
-- a -> b -> (Parser a) -> Parser b
--a -> b -> (String -> [(a, String)]) -> (String, [(b, String)]
--(x -g-> g x) --> (P p inp = [(x, out)]) -> [(g x, out)])

-- String -> (a, String)
                           
instance Functor Parser where 
 fmap g p = P (\inp -> case parse p inp of [] -> []
                                           [(v,out)] -> [(g v, out)])

item3 = fmap (ord) item

--our objective is to use the monad parser as a monad to convert elements

instance Applicative Parser where
  pure (v) = P (\inp -> [(v,inp)])
--(Parser (a -> b))
--(String -> [(a ->b, String)])
--Parser a = String -> [(a, String)]  

--px = String -> [(a, String)]
--g = (a -> b)
--fmap g px = Parser b = String -> [(b, String)]
  pg <*> px = P (\inp -> case parse pg inp of  [] -> []
                                               [(g,out)] -> parse (Prelude.fmap g px) out)



instance Monad Parser where
 --(Parser a -> (a -> Parser b) -> Parser b)
  p >>= f = P (\inp -> case parse p inp of [] -> []
                                           [(v,out)] -> parse (f v) out)
-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
-- term >>= rest = P (\input -> [(v, out)] -> parse (rest v) ot
three:: Parser (Char, Char)
three =  do x <- item
            y <- item
            return (x,y)
            
             
instance Alternative Parser where
 empty = P (\inp -> [])
 p <|> q = P (\inp -> (case (parse p inp) of [] -> (parse q inp)
                                             [(v, out)] -> [(v, out)]))
   
sat::(Char -> Bool) -> Parser Char
sat p = do x <- item
           (if p x then return x else empty)  
           
digit::Parser Char
digit = sat (isDigit)                           

lower :: Parser Char
lower = sat isLower


upper :: Parser Char
upper = sat isUpper


letter :: Parser Char
letter = sat isAlpha


alphanum :: Parser Char
alphanum = sat isAlphaNum
--quite often, we apply a command and it gives a relavent entry and then we want to further parse and check, if it does not
-- "124 + 56 *78 "

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


ident :: Parser String
ident = do x <- lower 
           xs <- some alphanum
           return (x:xs)


nat :: Parser Int
nat = do xs <- some digit
         return (read xs)


space :: Parser ()
space = do many (sat isSpace)
           return ()    
   
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

--modify above for decimals using some

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v
             
             
identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int


symbol :: String -> Parser String
symbol xs = token (string xs)

    


nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do (symbol ",") 
                         natural)
          symbol "]"
          return (n:ns)


--Two Different

data Expr = Con Int | Bin Op Expr Expr
            deriving (Read, Show, Eq)
data Op = Plus | Minus
            deriving (Read, Show, Eq)

paren :: Parser a -> Parser a
paren p = do {symbol "("; x <- p; symbol ")"; return x}

expr :: Parser Expr
expr = token (constant <|> paren binary)

constant = do {n <- nat; return (Con n)}

binary = do {e1 <- expr;
             p <- op;
             e2 <- expr;
             return (Bin p e1 e2)}

op = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)

--Another attempt
expr2 = token (term >>= rest)
term = token (constant <|> paren expr2)
rest e1 = do {p <- op; e2 <- term; rest (Bin p e1 e2)} <|> return e1

--Natural Language Processing

data Sentence = STree NP VP
            deriving (Read, Show, Eq)
data NP = N Noun | NPhrase Der Noun  | Nphrase2 Noun PP 
            deriving (Read, Show, Eq)
data PP = Past1 P NP | Past2 P Noun -- past2 is uselsss but let it be 
            deriving (Read, Show, Eq)
data VP  = VPhrase Verb NP 
            deriving (Read, Show, Eq)
data Noun = Jack | Jill | Car | Hill
            deriving (Read, Show, Eq)
data Verb = Plays  | Runs | Drives
            deriving (Read, Show, Eq)
data P = In | On
            deriving (Read, Show, Eq)
data Der = The | A
            deriving (Read, Show, Eq)

split = do {e1 <- nphrasec; p <- verbc; e2 <- nphrasec; return (STree e1 (VPhrase p e2))} 

verbc = (symbol "plays" >> return Plays) <|> (symbol "runs" >> return Runs)
         <|> (symbol "drives" >> return Drives)

nphrasec = token ( do { p <- derc; e2 <- nounc; return (NPhrase p e2)} <|> do {p <- nounc; e <- partc; return (Nphrase2 p e)} <|> do { p <- nounc; return (N p)})

partc = token (do {p <- prepc; np <- nounc; return (Past2 p np)} <|> do { p<- prepc; np <- nphrasec; return (Past1 p np)})  

prepc = (symbol "on" >> return On) <|> (symbol "in" >> return In)

nounc = (symbol "Jack" >> return Jack) <|> (symbol "Jill" >> return Jill) <|> (symbol "car" >> return Car) <|> (symbol "hill" >> return Hill)


derc = (symbol "The" >> return The) <|> (symbol "A" >> return A) <|> (symbol "a" >> return A) <|>  (symbol "the" >> return The)
   
decompose::String -> Sentence
decompose str = let y = head (parse split str) 
                    in (if ((snd y)==[]) then fst y else error "not fully parsed") 
                      



data F = F String Var deriving (Show, Read)
data Var = V String | V' Var Var deriving (Show, Read)

data Rule = R1 String Var F | R2 String Var Rule deriving (Show, Read)

opP :: Parser Char
opP = char '('

parseFact :: Parser F
parseFact = do e1 <- some alphanum ; char '(' ; e2 <- some alphanum ; char ')'; char '.'; return (F e1 (V e2))

parseRule = (do e1 <- some alphanum ; char '(' ; e2 <- some alphanum ; char ')' ; string ":-" ; e3 <- parseRule; return (R2 e1 (V e2) e3) ) <|> (do e1 <- some alphanum ; char '(' ; e2 <- some alphanum ; char ')' ; string ":-" ; e3 <- parseFact ; return (R1 e1 (V e2) e3) ) 


decompose4::String -> [F] 
decompose4 str = let y = head (parse parseFact str) 
                     in (if ((snd y)==[]) then [fst y] else (fst y):(decompose4 (snd y)))
                     
decompose5::String -> [Rule] 
decompose5 str = let y = head (parse parseRule str) 
                     in (if ((snd y)==[]) then [fst y] else (fst y):(decompose5 (snd y)))







