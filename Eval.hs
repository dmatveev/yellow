module Eval where


import Form


eval :: Form -> Either String Form
eval (SExpr o fs) = evalSExpr o fs
eval form         = return $ form
    

eval' :: String -> Either String Form
eval' s = do
  tree <- parseTree s
  eval tree

    
evalSExpr :: Operation -> [Form] -> Either String Form
evalSExpr op = case op of
    "+"    -> evalArith (+)
    "-"    -> evalArith (-)
    "*"    -> evalArith (*)
    
    "/"    -> evalBinary evalDiv
    "="    -> evalBinary evalEqual
    
    "if"   -> evalIf
    
    "cons" -> evalBinary evalCons
    "car"  -> evalUnary  evalCar
    "cdr"  -> evalUnary  evalCdr


evalArith :: (Double -> Double -> Double) -> [Form] -> Either String Form
evalArith f (arg : []) = do
  value <- numeric =<< eval arg
  return $ NumericLiteral $ value
  
evalArith f (arg : args) = do
  left  <- numeric =<< eval arg
  right <- numeric =<< evalArith f args
  return $ NumericLiteral $ f left right



evalUnary :: (Form -> Either String Form)
          -> [Form]
          -> Either String Form
evalUnary e (a:[]) = e a             
evalUnary _ _      = wrongNumberOfArgs


evalBinary :: (Form -> Form -> Either String Form)
           -> [Form]
           -> Either String Form
evalBinary e (a:b:[]) = e a b
evalBinary _ _        = wrongNumberOfArgs

    
evalDiv :: Form -> Form -> Either String Form
evalDiv a b = do
  a' <- numeric =<< eval a 
  b' <- numeric =<< eval b
  return $ NumericLiteral $ a' / b'


evalEqual :: Form -> Form -> Either String Form
evalEqual a b = do
  a' <- eval a
  b' <- eval b
  return $ BooleanLiteral $ a' == b'
  
  
evalIf :: [Form] -> Either String Form
evalIf (exp : thenForm : elseForm : _ ) = do
  e <- eval exp
  eval $ if e /= nil then thenForm else elseForm
    
evalIf (exp : thenForm : [] ) = evalIf [exp, thenForm, nil]
evalIf _ = wrongNumberOfArgs


evalCons :: Form -> Form -> Either String Form
evalCons h t = do
  h' <- eval h
  t' <- eval t
  return $ Cons h' t'


evalCar :: Form -> Either String Form
evalCar f = eval f >>= cons >>= \(h, _) -> return h


evalCdr :: Form -> Either String Form
evalCdr f = eval f >>= cons >>= \(_, t) -> return t


wrongNumberOfArgs :: Either String Form
wrongNumberOfArgs = Left "wrong number of arguments"