module Parse where 
import Expr
import Text.Parsec
import Data.Functor (($>))
import Control.Applicative (liftA2)

ws :: Parsec String u ()
-- TODO add comments
-- that was giving me issues for some reason...
ws = spaces

-- quick hack to allow math operators to be used prefix
-- should be removed once infix expressions are added.
parseId :: Parsec String u String
parseId = 
  try (string "+" <|> string "-" <|> string "*" <|> string "/" <|> string "==") <|> 
    liftA2 (:) letter (option "" (many1 alphaNum))

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

parseLetRec :: Parsec String u Expr
parseLetRec = do
  string "letrec"
  ws 
  name <- parseId
  ws
  char '='
  ws
  x <- parseExpr
  ws
  string "in"
  ws
  Letrec name x <$> parseExpr 

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

-- TODO add infix/math operators
parseApp :: Parsec String u Expr
parseApp = foldl1 App <$> liftA2 (:) parseAtom (many1 ((parseLit <|> parseAtom) <* ws))

-- the bools might get parsed as Ids without this seperate category
parseLit = ws *> (parseBool <|> parseChar <|> parseInt) <* ws

parseAtom = ws *> (Var <$> parseId <|> between (char '(') (char ')') parseExpr ) <* ws

parseExpr :: Parsec String u Expr
parseExpr = 
  ws *> 
    choice [parseLet, parseLetRec, parseIfThenElse, parseLambda, parseLit, parseApp, parseAtom] 
        <* ws
