{-# LANGUAGE BangPatterns #-}
module Parse where 
import Expr
import Text.Parsec
import Text.Parsec.Expr
import Data.Functor (($>))
import Control.Applicative (liftA2)
-- import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Identity

ws :: Parsec String u ()
-- TODO add comments
-- that was giving me issues for some reason...
ws = spaces

isKeyword = flip elem ["in","λ","let","letrec","if","then","else","True", "False"]
parseId :: Parsec String u String
parseId = 
    try (liftA2 (:) letter (option "" (many1 alphaNum)) >>= \x -> if isKeyword x then fail "ERROR: keyword used as id" else return x)

parseLambda :: Parsec String u Expr
parseLambda = do
  char 'λ' <|> char '\\'
  ws
  name <- parseId
  ws
  string "->"
  ws
  Lam name <$> parseExpr

parseLet :: Parsec String u Expr
parseLet = do
  string "let"
  ws 
  name <- parseId
  ws
  char '='
  ws
  x <- parseExpr
  ws
  string "in"
  ws
  Let name x <$> parseExpr

parseIfThenElse :: Parsec String u Expr
parseIfThenElse = do
  string "if"
  ws
  c <- parseExpr
  ws
  string "then"
  ws
  t <- parseExpr
  ws
  string "else"
  ws
  IfThenElse c t <$> parseExpr

parseBool :: Parsec String u Expr
parseBool = (string "True" $> Bool True) <|> (string "False" $> Bool False)

--TODO escape Characters
parseChar :: Parsec String u Expr
parseChar = Char <$> between (char '\'') (char '\'') (noneOf "\'")

parseInt :: Parsec String u Expr
parseInt = Int . read <$> many1 digit 

parseApp :: Parsec String u Expr
parseApp = foldl1 App <$> (parseAtom >>= \atom -> many ((parseLit <|> parseAtom) <* ws) >>= \rest-> return (atom:rest))

-- bools might get parsed as Ids without this seperate category
parseLit = ws *> (parseBool <|> parseChar <|> parseInt) <* ws

parseAtom = ws *> (between (char '(') (char ')') parseExpr <|> Var <$> parseId ) <* ws

parseExpr' :: Parsec String u Expr
parseExpr' = 
  ws *> 
    choice [parseLet, parseIfThenElse, parseLambda, parseLit, parseApp, parseAtom] 
        <* ws

parseExpr = buildExpressionParser table parseExpr'

-- might eventually add user-defined infix exprs?
-- would be a bit of a pain...
-- TODO impl unary negation
table = [ [prefix "-" (App (Var "-"))]
         -- , [postfix "++" (+1)]
         , [binary "*" (binApp "*") AssocLeft, binary "/" (binApp "/") AssocLeft ]
         , [binary "+" (binApp "+") AssocLeft, binary "-" (binApp "-") AssocLeft ]
         , [binary "==" (binApp "==") AssocNone]
         ]

binApp n x = App (App (Var n) x)

binary  :: String -> (a->a->a) -> Assoc -> Operator String u Identity a
binary   name fun = Infix  $ try (string name) >> pure fun

prefix  :: String -> (a->a) -> Operator String u Identity a 
prefix   name fun = Prefix $ try (string name) >> pure fun

postfix :: String -> (a->a) -> Operator String u Identity a
postfix  name fun = Prefix $ try (string name) >> pure fun

