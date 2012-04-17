module Form where


import Text.ParserCombinators.Parsec
import Data.List (intersperse)

type Operation = String

data Form = Symbol String
          | StringLiteral String 
          | NumericLiteral Double
          | BooleanLiteral Bool
          | SExpr Operation [Form]
          | Cons Form Form
            deriving (Eq)

instance Show Form where
    show = textify


wrapped :: Parser Form -> Parser Form
wrapped p = do
  optional spaces
  f <- p
  optional spaces
  return f

                     
parseForm :: Parser Form
parseForm = wrapped $
      parseSymbol
  <|> parseString
  <|> parseNumber
  <|> parseBool
  <|> parseSExpr


parseSymbol :: Parser Form
parseSymbol = do
  char '\''
  s <- many1 $ letter <|> digit
  return $ Symbol s
  
  
parseString :: Parser Form
parseString = do
  char '\"'
  s <- many $ noneOf ['\"']
  char '\"'
  return $ StringLiteral s
    

parseNumber :: Parser Form
parseNumber = do
  i <- many1 digit
  return $ NumericLiteral $ read i
  

parseBool :: Parser Form
parseBool = do
  s <- string "t" <|> string "nil"
  return $ BooleanLiteral $ s == "t"


parseSExpr :: Parser Form
parseSExpr = do
  char '('
  oper <- parseOperation
  optional spaces
  args <- parseForm `sepBy` spaces
  char ')'
  return $ SExpr oper args
  

parseOperation :: Parser String
parseOperation =   string "+"
               <|> string "-"
               <|> string "*"
               <|> string "/"
               <|> string "="
               <|> string ">="
               <|> string "<="
               <|> string ">"
               <|> string "<"
               <|> parseIdentifier

  
parseIdentifier :: Parser String
parseIdentifier = do
  start <- many1 letter
  end   <- many $ letter <|> digit
  return $ start ++ end


parseTree :: String -> Either String Form
parseTree s = case parse parseForm "" s of
  (Left err) -> Left $ show err
  (Right f)  -> return f
  

nil :: Form
nil = BooleanLiteral False


textify :: Form -> String
textify (Symbol s)         = '\'' : s
textify (StringLiteral s)  = concat ["\"", s, "\""]
textify (NumericLiteral n) = show n
textify (BooleanLiteral b) = if b then "t" else "nil"
textify (Cons a b)         = concat ["(", textify a, " . ", textify b, ")"]
textify (SExpr op fs)      = concat ["(", op, " ", args, ")"]
  where args = concat . intersperse " " . map textify $ fs