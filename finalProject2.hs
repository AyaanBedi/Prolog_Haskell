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




data Sentence = Fact F | Rule R deriving (Show, Read,Eq)
data F = F String [Var] deriving (Show, Read, Eq)
data Var = V String | V' Var Var deriving (Show, Read,Eq)

data R = R1 String [Var] [F] | R2 String [Var] R deriving (Show, Read,Eq)

opP :: Parser Char
opP = char '('

parseVar :: Parser Var
parseVar = token (do v1 <- some alphanum; return (V v1))
       <|> token (do char ','; v2 <- some alphanum; return (V v2))

decomposeVar :: String -> [Var]
decomposeVar str = let y = head (parse parseVar str)
                     in (if ((snd y)==[]) then [fst y] else (fst y):(decomposeVar (snd y)))

parseUntilClosingBracket :: Parser String
parseUntilClosingBracket = P $ \inp -> case span (/= ')') inp of
    (parsed, rest) -> [(parsed, dropWhile (== ')') rest)]

parseUntilFullStop :: Parser String
parseUntilFullStop = P $ \inp -> case span (/= '.') inp of
    (parsed, rest) -> [(parsed ++ ".", dropWhile (== '.') rest)]

parseFact :: Parser F
parseFact = token (do e1 <- some alphanum ; char '(' ; e2 <- parseUntilClosingBracket ; char '.'; return (F e1 (decomposeVar e2)))
        <|> token (do e1 <- some alphanum ; char '(' ; e2 <- parseUntilClosingBracket ; char '.'; char '\n'; return (F e1 (decomposeVar e2)))

parseFactForRule :: Parser F
parseFactForRule = token (do e1 <- some alphanum ; char '(' ; e2 <- parseUntilClosingBracket ; char ','; return (F e1 (decomposeVar e2)))
        <|> token (do e1 <- some alphanum ; char '(' ; e2 <- parseUntilClosingBracket ; char ','; char '\n'; return (F e1 (decomposeVar e2)))
        <|> token (do e1 <- some alphanum ; char '(' ; e2 <- parseUntilClosingBracket ; char '.'; return (F e1 (decomposeVar e2)))
        <|> token (do e1 <- some alphanum ; char '(' ; e2 <- parseUntilClosingBracket ; char '.'; char '\n'; return (F e1 (decomposeVar e2)))

decomposeFactForRule::String -> [F] 
decomposeFactForRule str = let y = head (parse parseFactForRule str) 
                     in (if ((snd y)==[]) then [fst y] else (fst y):(decomposeFactForRule (snd y)))

parseRule =  token ((do e1 <- some alphanum ; char '(' ; e2 <- parseUntilClosingBracket ; string ":-" ; e3 <- parseRule; return (R2 e1 (decomposeVar e2) e3) ) 
  <|> (do e1 <- some alphanum ; char '(' ; e2 <- parseUntilClosingBracket ; string ":-" ; e3 <- parseUntilFullStop ; return (R1 e1 (decomposeVar e2) (decomposeFactForRule e3)) ) )

parseStatement = token ( (do e1 <- parseRule ; return (Rule e1)) <|> (do e1 <- parseFact ; return (Fact e1) )) 

decompose4::String -> [F] 
decompose4 str = let y = head (parse parseFact str) 
                     in (if ((snd y)==[]) then [fst y] else (fst y):(decompose4 (snd y)))
                     
decompose5::String -> [R] 
decompose5 str = let y = head (parse parseRule str) 
                     in (if ((snd y)==[]) then [fst y] else (fst y):(decompose5 (snd y)))
                     
decomposeSatement :: String -> [Sentence] 
decomposeSatement str = let y = head (parse parseStatement str) 
                        in (if ((snd y)==[]) then [fst y] else (fst y):(decomposeSatement (snd y)))

fulldecomp:: [Sentence] -> ([F] , [R])
fulldecomp [] = ([],[])
fulldecomp ((Fact x):xs) = let (a,b) = (fulldecomp xs )
                                in 
                                (((x):a) , b)                   
fulldecomp ((Rule x):xs) = (a , (x:b)) where (a,b) = fulldecomp xs

notnothing:: Maybe F -> Bool 
notnothing Nothing = False 
notnothing _ = True 

factList = filter(\x -> notnothing x)
{-
searchRules :: F -> [R] -> Bool
searchRules _ [] = False
searchRules (F queryName queryVars) ((R1 ruleName ruleVars ruleFacts):ruleList) 
   | queryName==ruleName && (length queryVars)==(length ruleVars) = checkRule (F queryName queryVars) (R1 ruleName ruleVars ruleFacts)
   | otherwise = searchRules (F queryName queryVars) ruleList

checkRule :: F -> R -> Bool
checkRule (F _ (qv:queryVars)) (R1 _ (rv:ruleVars) ((F ruleFactName ruleFactVars):ruleFacts)) = 

findRuleVarsInRuleFact :: R -> [F]
findRuleVarsInRuleFact (R1 _ _ []) = []
findRuleVarsInRuleFact (R1 x (rv:ruleVars) ((F ruleFactName ruleFactVars):ruleFacts)) = if rv `elem` ruleFactVars 
   then (F ruleFactName ruleFactVars):findRuleVarsInRuleFact (R1 x (rv:ruleVars) ruleFacts)
   else findRuleVarsInRuleFact (R1 x (rv:ruleVars) ruleFacts)
-}

{-
test:
fact:
male(sam).
male(bob).
parent(sam,bob).
rule:
father(x,y):-male(x),parent(x,y).
after parsing = [Fact (F "male" [V "sam"]),
                 Fact (F "male" [V "bob"]),
                 Fact (F "parent" [V "sam",V "bob"]),
                 Rule (R1 "father" [V "x",V "y"] [F "male" [V "x"],F "parent" [V "x",V "y"]])]
query: 
  father(sam,bob).
  parsed = [Fact (F "father" [V "sam",V "bob"])]

check through factlist, fail
check through rule list, match with father(x,y)
  [Fact (F "FATHER" [V "sam",V "bob"])] = Rule (R1 "FATHER" [V "x",V "y"] [F "male" [V "x"],F "parent" [V "x",V "y"]])]
equate x and y to sam and bob
Rule (R1 "father" [V "sam",V "bob"] [F "male" [V "x"],F "parent" [V "x",V "y"]])]
how do you equate it in the facts for the rule???
1. given ruleVars, find facts that use that ruleVar. in this case: Rule (R1 "father" [V "x",V "y"] [F "male" [V "x"],F "parent" [V "x",V "y"]])]
      x is in male and parent. for male, it is simple as only x is present. for parent, x and y are present.
2. replace all vars with the query vars
      number of lists = 3
      list 1 - query vars: [V "sam",V "bob"]
      list 2 - rule vars: [V "x",V "y"]
      list 3 - fact vars in the rule: [[V "x"],[V "x",V "y"]] (first one for male, second for parent)

      to replace, go through each rule var in the list of lists and replace with corresponding query var and return the whole list,
      then move on to the next rule var and ciontinue. return the whole list of facts and search for those facts
-}
checkRuleMatch :: F -> [R] -> [R]
checkRuleMatch _ [] = []
checkRuleMatch (F queryName factVars) ((R1 ruleName ruleVars ruleFacts):rules) = if queryName==ruleName && length factVars==length ruleVars
      then R1 ruleName ruleVars ruleFacts:checkRuleMatch (F queryName factVars) rules
      else checkRuleMatch (F queryName factVars) rules

replaceElement :: Eq a => a -> a -> [a] -> [a]
replaceElement old new = map (\x -> if x == old then new else x)

replaceVar :: Var -> Var -> [F] -> [F]
replaceVar _ _ [] = []
replaceVar old new ((F x varList):factList) = F x (replaceElement old new varList):replaceVar old new factList

replaceVarsFromRule :: F -> R -> [F]
replaceVarsFromRule (F _ []) (R1 _ [] factList) = factList
replaceVarsFromRule (F x (qv:queryVars)) (R1 y (rv:ruleVars) factList) = replaceVarsFromRule (F x queryVars) (R1 y ruleVars (replaceVar rv qv factList))

replaceVarsFromAllRules :: F -> [R] -> [[F]]
replaceVarsFromAllRules _ [] = []
replaceVarsFromAllRules fact (r:rs) = replaceVarsFromRule fact r:replaceVarsFromAllRules fact rs  

search2 :: [F] -> [F] -> Bool
search2 derivedFacts fileFacts = all (`elem` fileFacts) derivedFacts

search3 :: [[F]] -> [F] -> Int
search3 [] _ = 0
search3 (fs:fss) fileFacts = if search2 fs fileFacts then 1 + search3 fss fileFacts else search3 fss fileFacts


search1 :: F -> [F] -> Bool
search1 _ [] = False
search1 query (x:fileFacts) = if x==query then True else search1 query fileFacts

main :: IO()
main = do
  contents <- readFile "family.pl"
  putStrLn "?> " 
  query0 <- getLine
  let query = head (decompose4 query0)
      factsAndRules = fulldecomp (decomposeSatement contents)
      facts = fst factsAndRules
      rules = snd factsAndRules
      ruleMatches = checkRuleMatch query rules
      replacedRules = replaceVarsFromAllRules query ruleMatches
  if search1 query facts || search3 replacedRules facts > 0
    then print "True"
    else print "False"
{-
fulldecomp:: [Sentence] -> ([Maybe F] , [R])
fulldecomp [] = ([],[])
fulldecomp ((Fact x):xs) = let (a,b) = (fulldecomp xs )
                                in 
                                (((Just x):a) , b)                   
fulldecomp ((Rule x):xs) = (a , (x:b)) where (a,b) = fulldecomp xs

finaldecomp:: [Maybe F] -> [R] -> [Maybe F] 
finaldecomp [] r = []
finaldecomp (((Just f)):xs) r = let {y = map (ruletofact f) r ; y2 = finaldecomp xs r } in (((Just f):y)++y2) 
finaldecomp ((Nothing):xs) r = finaldecomp xs r


ruletofact:: F -> R -> (Maybe F )
ruletofact (F s3 v2) (R1 s rVar (F s2 v1)) = if (s2==s3) then (Just (F s v2)) else Nothing

--ruletofact2 :: F -> R -> Maybe F
--ruletofact2 (F factName factVars) (R1 ruleName ruleVars ruleFact) = 

search :: [Maybe F] -> F -> Bool
search [] _ = False
search (x:fileContents) query = if x == (Just query) then True else search fileContents query

main :: IO ()
main = do
  contents <- readFile "family.pl"
  putStrLn "?> " 
  query <- getLine 
  let x = fulldecomp (decomposeSatement contents)
      result = search (finaldecomp (fst x) (snd x)) (head (decompose4 query))
  print result

{-
searching <- print (search (finaldecomp (fst x) (snd x)) (head (decompose4 query)))
   where 
    x = (fulldecomp (decomposeSatement contents))
-}
-}