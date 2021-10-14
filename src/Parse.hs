{-# LANGUAGE BangPatterns #-}
module Parse where 
import Expr
import Text.Parsec
import Text.Parsec.Expr
import Data.Functor (($>))
import Control.Applicative (liftA2)
-- import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Identity

--TODO(s)
-- add comments
-- add mutually recursive defs/lets
-- add first class list parsing

ws :: Parsec String u ()
-- TODO add comments
-- that was giving me issues for some reason...
ws = spaces

parseProgram :: Parsec String u [Either (String, Type) (String,Expr)]
parseProgram = many (parseFuncDef <|> parseTypeAnnotation)


parseTypeAnnotation :: Parsec String u (Either (String, Type) b)
parseTypeAnnotation = do
    name <- parseId 
    ws
    _ <- string "::"
    ws
    ty <- parseType
    pure $ Left (name,ty)

parseType :: Parsec String u Type
parseType = 
    between ws ws
        ((string "Integer" >> pure Integer) 
            <|> (string "Character" >> pure Character) 
            <|> between (char ')') (char ')') parseType
            <|> (parseConstructor >>= \c -> ws >> many parseType >>= \tys -> pure (Constructor c tys))
            <|> foldl1 Arrow <$> many1 (parseType >>= \ty -> ws >> string "->" >> ws >> pure ty) )



parseConstructor :: Parsec String u String
parseConstructor = liftA2 (:) upper (many alphaNum)

--TODO ability to define multiple functions at once for mutual recursion
--TODO arguements (duh)
parseFuncDef :: Parsec String u (Either a (String, Expr))
parseFuncDef = do
  _ <- string "def" 
  ws
  name <- parseId
  ws
  _ <- char '='
  ws
  body <- parseExpr
  pure $ Right (name, body)

isKeyword :: String -> Bool
isKeyword = flip elem ["in","λ","let","letrec","if","then","else", "match", "with", "def"]
parseId :: Parsec String u String
parseId = 
    try (liftA2 (:) lower (option "" (many1 alphaNum)) >>= \x -> if isKeyword x then fail $ "ERROR: keyword " <> x <> " used as id" else pure x)

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
  pure $ Match c [(PCon "True" [], t),(PCon "False" [], e)]


parsePattern :: Parsec String u Pattern
parsePattern = 
    ws *> ( 
        (char '_' >> pure PWild) <|> 
        (char '[' >> foldl (\x y -> PCon "Cons" [y,x]) (PCon "Nil" []) <$> (parsePattern `sepBy` (ws >> char ',' >> ws)) <* char ']') <|>
        (PAtom . AInt <$> (read <$> many1 digit)) <|>
        (PAtom . AChar <$> between (char '\'') (char '\'') (noneOf "\'")) <|>
        (liftA2 (:) upper (many alphaNum) >>= \c -> ws >> many parsePattern >>= \a -> pure $ PCon c a) <|>
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
    pure $ Match e b
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
                pure (p,e)

--TODO escape Characters
parseChar :: Parsec String u Expr
parseChar = Char <$> between (char '\'') (char '\'') (noneOf "\'")

parseInt :: Parsec String u Expr
parseInt = Int . read <$> many1 digit 

parseCon :: Parsec String u Expr
parseCon = do
    ws
    c <- parseConstructor
    ws
    a <- try $ many parseExpr
    ws
    pure $ Con c a

parseList :: Parsec String u Expr
parseList = do
    ws
    _ <- char '['
    content <- foldr (\x y -> Con "Cons" [x,y]) (Con "Nil" []) <$> (parseExpr `sepBy` (ws >> char ',' >> ws))
    _ <- char ']'
    pure content


parseApp :: Parsec String u Expr
parseApp = foldl1 App <$> (parseAtom >>= \atom -> many ((parseLit <|> parseAtom) <* ws) >>= \rest-> pure (atom:rest))

parseLit :: Parsec String u Expr
parseLit = ws *> (parseList <|> parseChar <|> parseInt <|> parseCon) <* ws

parseAtom :: Parsec String u Expr
parseAtom = ws *> (between (char '(') (char ')') parseExpr <|> Var <$> parseId ) <* ws

parseExpr' :: Parsec String u Expr
parseExpr' = 
  ws *> 
    choice [try parseLet, parseLambda, parseLit, try parseIfThenElse, try parseMatch, parseApp, parseAtom]
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
