import Control.Applicative
import Data.Char

import Control.Monad (replicateM)

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) s = p s

item :: Parser Char
item = P (\inp -> case inp of
  [] -> []
  (x:xs) -> [(x, xs)])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case (parse p inp) of
    [] -> []
    [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\inp -> [(x,  inp)])
  
  -- (<*>) :: Parser (a->b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case (parse pg inp) of
    [] -> []
    [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= g = P (\inp -> case parse p inp of
    [] -> []
    [(v, out)] -> parse (g v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (const [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
    [] -> parse q inp
    [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat pred = do
  x <- item
  if pred x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

alpha :: Parser Char
alpha = sat isAlpha

char :: Char -> Parser Char
char x = sat (x==)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

-- identifier as in code (token)
ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x:xs)

nat :: Parser Int
nat = fmap read (some digit)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Int
int = do
  char '-'
  x <- nat
  return (-x)
  <|> nat

token :: Parser a -> Parser a
token p = do
  space
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
symbol s = token (string s)

nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <- many (do
    symbol ","
    natural)
  symbol "]"
  return (n:ns)

surround :: String -> String -> Parser a -> Parser a
surround start end p =
  do
    symbol start
    r <- p
    symbol end
    return r

success :: a -> Parser a
success x = (\_ -> x) <$> return ()

fail :: a -> Parser a
fail _ = P(\_ -> [])

-- rpn :: String -> String
-- rpn ins = 

-- +, -, *, /, ^
data Expr = Expr STerm | Add STerm Expr deriving Show
data STerm = S Factor | Sub Factor STerm deriving Show
data Factor = F Dfactor | Mul Dfactor Factor deriving Show
data Dfactor = D Exp | Div Exp Dfactor deriving Show
data Exp = E Expr2 | Exp Expr2 Exp deriving Show
data Expr2 = Expr2 Expr | Val Char deriving Show

expr :: Parser Expr
expr = do
  st <- sterm
  (do
    symbol "+"
    e <- expr
    return (Add st e)) <|> return (Expr st)

sterm :: Parser STerm
sterm = do
  f <- factor
  (do
    symbol "-"
    st <- sterm
    return (Sub f st)) <|> return (S f)

factor :: Parser Factor
factor = do
  df <- dfactor
  (do
    symbol "*"
    f <- factor
    return (Mul df f)) <|> return (F df)

dfactor :: Parser Dfactor
dfactor = do
  e <- exp_
  (do
    symbol "/"
    df <- dfactor
    return (Div e df)) <|> return (D e)

exp_ :: Parser Exp
exp_ = do
  e2 <- expr2
  (do
    symbol "^"
    e <- exp_
    return (Exp e2 e)) <|> return (E e2)

expr2 :: Parser Expr2
expr2 = do
  symbol "("
  e <- expr
  symbol ")"
  return (Expr2 e)
  <|> do
    c <- token lower
    return (Val c)

parseExpr :: String -> Expr
parseExpr s = case parse expr s of
  [] -> error "unable to parse"
  [(x,[])] -> x
  [(_,s)] -> error ("left over input" ++ s)
  

rpnExpr :: Expr -> String
rpnExpr (Expr st) = rpnSterm st
rpnExpr (Add st e) = (rpnSterm st) ++ (rpnExpr e) ++ "+"

rpnSterm (S f) = rpnFactor f
rpnSterm (Sub f st) = (rpnFactor f) ++ (rpnSterm st) ++ "-"

rpnFactor (F df) = rpnDfactor df
rpnFactor (Mul df f) = (rpnDfactor df) ++ (rpnFactor f) ++ "*"

rpnDfactor (D e) = rpnExp e
rpnDfactor (Div e df) = (rpnExp e) ++ (rpnDfactor df) ++ "/"

rpnExp (E e2) = rpnExpr2 e2
rpnExp (Exp e2 e) = (rpnExpr2 e2) ++ (rpnExp e) ++ "^"

rpnExpr2 (Expr2 e) = rpnExpr e
rpnExpr2 (Val c) = [c]

rpn :: String -> String
rpn = rpnExpr . parseExpr

-- parseInt :: String -> Int
-- parseInt s = case parse integer s of
--   [(x,[])] -> x

main = 
  do
    input <- getLine
    replicateM (read input) (do
      l <- getLine
      putStrLn $ rpn l)