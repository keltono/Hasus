module Parse where 
import Expr
import Text.Parsec

ws :: Parsec String u ()
-- TODO add comments
-- that was giving me issues for some reason...
ws = spaces
parseId :: Parsec String u String
parseId = 
  letter >>= \l1 -> option "" (many1 alphaNum) >>= \lr -> return (l1:lr)

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
parseBool = 
  (string "True" >> return (Bool True)) <|>
    (string "False" >> return (Bool False))

--TODO escape Characters
parseChar :: Parsec String u Expr
parseChar = char '\'' >> noneOf "\'" >>= \c -> char '\'' >> return (Char c)

parseInt :: Parsec String u Expr
parseInt = many1 digit >>= \i -> return $ Int $ read i

parseApp :: Parsec String u Expr
parseApp = foldl1 App <$> many1 parseAtom

parseAtom = Var <$> parseId  <|> between (char '(') (char ')') parseExpr

parseExpr :: Parsec String u Expr
parseExpr = (ws >> char '(' >> parseExpr >>= \e -> char ')' >> ws >> return e) <|> (ws >> choice [parseLambda, parseLet, parseLetRec, parseIfThenElse, parseBool, parseChar, parseInt, parseApp, parseAtom]>>= \c -> ws >> return c)
