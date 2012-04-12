module Eval where


import Form


eval :: Form -> Either String Form
eval (SExpr o fs) = evalSExpr o fs
eval form         = return $ form
    
    
evalSExpr :: Operation -> [Form] -> Either String Form
evalSExpr op args =
  case op of
    "+" -> evalArith (+) args
    "-" -> evalArith (-) args
    "*" -> evalArith (*) args
    "/" -> do if length args /= 2
              then Left $ "/ is a binary operation!"
              else evalDiv (args !! 0) (args !! 1)


evalArith :: (Double -> Double -> Double) -> [Form] -> Either String Form
evalArith f (arg : []) = do
  value <- numeric =<< eval arg
  return $ NumericLiteral $ value
  
evalArith f (arg : args) = do
  left  <- numeric =<< eval arg
  right <- numeric =<< evalArith f args
  return $ NumericLiteral $ f left right

    
evalDiv :: Form -> Form -> Either String Form
evalDiv a b = do
  a' <- numeric =<< eval a 
  b' <- numeric =<< eval b
  return $ NumericLiteral $ a' / b'
    
    
eval' :: String -> Either String Form
eval' s = do
  tree <- parseTree s
  eval tree