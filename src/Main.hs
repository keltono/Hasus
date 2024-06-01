module Main where
import Eval
-- import Typing.Type
import Expr
import Parse
import Text.Parsec
import System.Environment 
import Data.List (partition, groupBy) 

--- Problems:
--- Parse data doesn't slot in well to inference, or eval.
--- Inference: no way to forward ParseType to Inference use, or Expr to TyExpr
--- Inference Soln: Just make transforms, feed annotations into where they go

parseString :: String -> String -> IO [ParseDecl]
parseString name src  =
  case parse (ws *> parseProgram <* eof ) name src of
    Left e -> do
      print e
      error "parseError"
    Right t ->
        --ignore the type annotations for now
        pure t


parseStdin :: IO [ParseDecl]
parseStdin = parseString "(stdin)" =<< getContents

parseFile :: FilePath -> IO [ParseDecl]
parseFile x = parseString x =<< readFile x

parseAndEval ::  String -> Expr
parseAndEval src = 
    case parse (ws *> parseExpr <* eof) "file" src of
    Left _ ->  error "parseError"
    Right e -> case inter e of
                 Right r -> r
                 Left er  -> error $ "eval error " <> er

funcDefs :: [ParseDecl] -> [(String, [Pattern], Expr)]
funcDefs = foldr comb []  
    where
        comb (FuncDef s ps e) l =  (s,ps,e) : l
        comb _ l = l


main :: IO ()
main = do 
  args <- getArgs
  -- if no arguments, read from stdin. Otherwise, read from the given file.
  -- this obviously doesn't work with command line options, 
  -- but it also will not be hard to rewrite if/when those are added
  prog <- if null args then parseStdin else parseFile $ head args
  let (mainDef, others) = partition (\(name,_,_) -> name == "main") (funcDefs prog) in
      if null mainDef
         then putStrLn "ERROR: program has no main function"
         else
             -- This method doesn't allow functions to be used before they are defined, or mutual recursion
             -- The second issue will be solved eventually, in a manner like ocaml (but with better keywords than "let" and "and")
             let program = foldr (\(name,body) rest -> Let name body rest) (funcDefToMatch mainDef) $
                    map (\f@((n,_,_):_) -> (n,funcDefToMatch f)) (groupBy (\(a,_,_) (a',_,_) -> a==a') others) in
                     print program >> print (quote $ eval startVEnv program)
             -- print $ map (\(s,t) -> s ++ " : " ++ show t ) strTypeList 

