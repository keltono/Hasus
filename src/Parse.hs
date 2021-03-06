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

parseProgram :: Parsec String u [(String,Expr)]
parseProgram = many parseFuncDef

parseFuncDef :: Parsec String u (String,Expr)
parseFuncDef = do
  _ <- string "def" 
  ws
  name <- parseId
  ws
  _ <- char '='
  ws
  body <- parseExpr
  return (name,body)

isKeyword :: String -> Bool
isKeyword = flip elem ["in","λ","let","letrec","if","then","else", "match", "with"]
parseId :: Parsec String u String
parseId = 
    try (liftA2 (:) lower (option "" (many1 alphaNum)) >>= \x -> if isKeyword x then fail $ "ERROR: keyword " <> x <> " used as id" else return x)

parseLambda :: Parsec String u Expr
parseLambda = do
  _ <- char 'λ' <|> char '\\'
  ws
  name <- parseId
  ws
  _ <- string "->"
  ws
  Lam name <$> parseExpr

parseLet :: Parsec String u Expr
parseLet = do
  _ <- string "let"
  ws 
  name <- parseId
  ws
  _ <- char '='
  ws
  x <- parseExpr
  ws
  _ <- string "in"
  ws
  Let name x <$> parseExpr

parseIfThenElse :: Parsec String u Expr
parseIfThenElse = do
  _ <- string "if"
  ws
  c <- parseExpr
  ws
  _ <- string "then"
  ws
  t <- parseExpr
  ws
  _ <- string "else"
  ws
  e <- parseExpr
  return $ Match c [(PCon "True" [], t),(PCon "False" [], e)]


parsePattern :: Parsec String u Pattern
parsePattern = 
    ws *> ( 
        (char '_' >> return PWild) <|> 
        (PAtom . AInt <$> (read <$> many1 digit)) <|>
        (PAtom . AChar <$> between (char '\'') (char '\'') (noneOf "\'")) <|>
        (liftA2 (:) upper (many alphaNum) >>= \c -> ws >> many parsePattern >>= \a -> return $ PCon c a) <|>
        (PVar <$> parseId) 
         ) <* ws

parseMatch :: Parsec String u Expr
parseMatch = do
    ws
    _ <- string "match"
    ws 
    e <- parseExpr
    ws
    _ <- string "with"
    b <- many1 parseMatchLine
    return $ Match e b
        where 
            parseMatchLine :: Parsec String u (Pattern,Expr)
            parseMatchLine = do
                ws 
                _ <- char '|'
                ws
                p <- parsePattern
                ws
                _ <- string "->"
                ws
                e <- parseExpr
                ws
                return (p,e)


-- parseBool :: Parsec String u Expr
-- parseBool = (string "True" $> Bool True) <|> (string "False" $> Bool False)

--TODO escape Characters
parseChar :: Parsec String u Expr
parseChar = Char <$> between (char '\'') (char '\'') (noneOf "\'")

parseInt :: Parsec String u Expr
parseInt = Int . read <$> many1 digit 

parseCon :: Parsec String u Expr
parseCon = do
    ws
    c <- liftA2 (:) upper (many alphaNum)
    ws
    a <- try $ many parseExpr
    ws
    return $ Con c a

parseApp :: Parsec String u Expr
parseApp = foldl1 App <$> (parseAtom >>= \atom -> many ((parseLit <|> parseAtom) <* ws) >>= \rest-> return (atom:rest))

-- bools might get parsed as Ids without this seperate category
parseLit :: Parsec String u Expr
parseLit = ws *> (parseChar <|> parseInt <|> parseCon) <* ws

parseAtom :: Parsec String u Expr
parseAtom = ws *> (between (char '(') (char ')') parseExpr <|> Var <$> parseId ) <* ws

parseExpr' :: Parsec String u Expr
parseExpr' = 
  ws *> 
    choice [parseLet, parseLambda, parseLit, parseIfThenElse, parseMatch, parseApp, parseAtom]
        <* ws

parseExpr :: Parsec String u Expr
parseExpr = buildExpressionParser table parseExpr'

-- might eventually add user-defined infix exprs?
-- would be a bit of a pain...
table :: [[Operator String  u Identity Expr ]]
table = [ [prefix "-" (App (Var "~"))]
         , [binary "*"  AssocLeft, binary "/" AssocLeft ]
         , [binary "+"  AssocLeft, binary "-" AssocLeft ]
         , [binary "==" AssocNone]
         ]


binary  :: String -> Assoc -> Operator String u Identity Expr
binary  name = Infix  $ try (string name) $> binApp name
  where binApp n x = App (App (Var n) x)

prefix  :: String -> (a->a) -> Operator String u Identity a 
prefix  name fun = Prefix $ try (string name) $> fun

postfix :: String -> (a->a) -> Operator String u Identity a
postfix name fun = Prefix $ try (string name) $> fun

matchTest :: String
matchTest = 
    "match Cons 1 Nil with \n\
        \| Nil -> 1 \n\
        \| Cons x _ -> x"
