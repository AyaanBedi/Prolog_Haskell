main :: IO () 
main = do 
   contents <- readFile "family.pl" 
   putStrLn contents 