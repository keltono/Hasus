module Main where
import Eval
import Type
import Expr
import Parse
import Text.Parsec
-- main = putStrLn "Hello!"
-- main = 
--   case parse 

parseString :: String -> IO Expr
parseString src =
  case parse (ws *> parseExpr <* eof ) "(stdin)" src of
    Left e -> do
      print e
      error "parseError"
    Right t ->
      pure t

parseStdin :: IO Expr
parseStdin = parseString =<< getContents

main :: IO ()
main = do 
  expr <- parseStdin 
  print $ typeInfer expr startEnv
  print $ interpret expr []

