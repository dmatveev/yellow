module Form where


import Text.ParserCombinators.Parsec


type Operation = String

data Form = Symbol String
          | StringLiteral String 
          | NumericLiteral Double
          | BooleanLiteral Bool
          | SExpr Operation [Form]
          | Cons Form Form
            deriving (Eq, Show)
                     

parseForm :: Parser Form
parseForm =   parseSymbol
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
               <|> parseIdentifier

  
parseIdentifier :: Parser String
parseIdentifier = do
  start <- many1 letter
  end   <- many $ letter <|> digit
  return $ start ++ end


parseTree :: String -> Either String Form
parseTree s = case parse parseForm "" s of
  (Left err) -> fail $ show err
  (Right f)  -> return f
  

nil :: Form
nil = BooleanLiteral False

numeric :: Form -> Either String Double
numeric (NumericLiteral i) = return i
numeric _                  = Left "number expected"


cons :: Form -> Either String (Form, Form)
cons (Cons h t) = return $ (h, t)
cons _ = Left "cons expected"
