module Parse where 
import Expr
import Text.Parsec

-- TODO it never parses applications now??!?!? 
-- it was working earlier and i didn't think anything had changed....
-- bruh
-- going to hope this is a whitespace issue

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
parseChar = char '\'' >> Char <$> (noneOf "\'" <* char '\'')

parseInt :: Parsec String u Expr
parseInt = many1 digit >>= \i -> return $ Int $ read i

parseApp :: Parsec String u Expr
parseApp = foldl1 App <$> many1 (parseAtom <* ws)

parseAtom = Var <$> parseId  <|> between (char '(') (char ')') parseExpr <|> parseChar <|> parseInt <|> parseBool

parseExpr :: Parsec String u Expr
parseExpr = 
  ws >> 
    choice [parseApp, parseLambda, parseLet, parseLetRec, parseIfThenElse, parseAtom] --, parseAtom]
        <* ws
    -- <|>
  -- (ws >> char '(' >> parseExpr >>= \e -> char ')' >> ws >> return e) 
